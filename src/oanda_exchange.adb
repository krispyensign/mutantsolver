pragma Ada_2022;

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Util.Dates.ISO8601;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
with Ada.Strings.Fixed;
with Ada.Calendar.Conversions;
with GNAT.MD5;
with Ada.Directories;
with GNAT.OS_Lib;

package body Oanda_Exchange is
   package ubo renames Ada.Strings.Unbounded;
   package fixed renames Ada.Strings.Fixed;
   package strings renames Ada.Strings;
   package io renames Ada.Text_IO;
   package conv renames Ada.Calendar.Conversions;
   package dir renames Ada.Directories;
   package os renames GNAT.OS_Lib;
   package md5 renames GNAT.MD5;

   function Construct_URL
     (oanda : Config.Oanda_Access; chart : Config.Chart_Config) return String
   is
      count           : constant Integer :=
        chart.Online_Set_Size + chart.Offline_Set_Size;
      constructed_url : constant String :=
        ubo.To_String (oanda.URL)
        & "/v3/instruments/"
        & chart.Instrument
        & "/candles?price=MAB&granularity="
        & chart.Granularity
        & "&count="
        & fixed.Trim (count'Image, strings.Both);
   begin
      return constructed_url;
   end Construct_URL;

   function Construct_URL
     (oanda   : Config.Oanda_Access;
      chart   : Config.Chart_Config;
      to_time : calendar.Time) return String
   is
      seconds         : constant Long_Long_Integer :=
        Long_Long_Integer (conv.To_Unix_Time_64 (to_time));
      constructed_url : constant String :=
        Construct_URL (oanda, chart)
        & "&to="
        & fixed.Trim (seconds'Image, strings.Both);
   begin
      return constructed_url;
   end Construct_URL;

   function Make_Candle (current_candle : json.JSON_Value) return Core.Candle
   is
      temp_string : ubo.Unbounded_String := current_candle.Get ("time");
   begin
      temp_string := ubo.Replace_Slice (temp_string, 20, 30, "Z");
      return
        (Volume    => current_candle.Get ("volume"),
         Complete  => current_candle.Get ("complete"),
         Ask_Open  => Long_Float'Value (current_candle.Get ("ask").Get ("o")),
         Ask_High  => Long_Float'Value (current_candle.Get ("ask").Get ("h")),
         Ask_Low   => Long_Float'Value (current_candle.Get ("ask").Get ("l")),
         Ask_Close => Long_Float'Value (current_candle.Get ("ask").Get ("c")),
         Mid_Open  => Long_Float'Value (current_candle.Get ("mid").Get ("o")),
         Mid_High  => Long_Float'Value (current_candle.Get ("mid").Get ("h")),
         Mid_Low   => Long_Float'Value (current_candle.Get ("mid").Get ("l")),
         Mid_Close => Long_Float'Value (current_candle.Get ("mid").Get ("c")),
         Bid_Open  => Long_Float'Value (current_candle.Get ("bid").Get ("o")),
         Bid_High  => Long_Float'Value (current_candle.Get ("bid").Get ("h")),
         Bid_Low   => Long_Float'Value (current_candle.Get ("bid").Get ("l")),
         Bid_Close => Long_Float'Value (current_candle.Get ("bid").Get ("c")),
         Time      => Util.Dates.ISO8601.Value (ubo.To_String (temp_string)));
   end Make_Candle;

   function Fetch_Candle_Data
     (token : String; constructed_url : String) return String is
   begin
      pragma Extensions_Allowed (On);

      Util.Http.Clients.Curl.Register;
      http : Util.Http.Clients.Client;
      response : Util.Http.Clients.Response;
      count : Natural := 1;

      loop
         --  setup headers
         http.Add_Header ("Content-Type", "application/json");
         http.Add_Header ("Authorization", "Bearer " & token);
         http.Get (constructed_url, response);
         if response.Get_Status = 200 then
            exit;
         end if;

         io.Put_Line (response.Get_Body);
         io.Put_Line (constructed_url);
         io.Put_Line ("error retrieving candles");
         count := count + 1;
         if count > 4 then
            raise Program_Error;
         end if;
      end loop;

      return response.Get_Body;
   end Fetch_Candle_Data;

   function Fetch_Candles
     (oanda      : Config.Oanda_Access;
      chart      : Config.Chart_Config;
      sys_conf   : Config.System_Config;
      date_index : Natural := 0) return Core.Candles
   is
      pragma Extensions_Allowed (On);
      unmapped_json_array : json.JSON_Array;
      count               : constant Integer :=
        chart.Online_Set_Size + chart.Offline_Set_Size;
      out_candles         : Core.Candles (1 .. count);
      constructed_url     : constant String :=
        (if date_index = 0 then Construct_URL (oanda, chart)
         else Construct_URL (oanda, chart, chart.Dates (date_index)));
      hashed_file_name    : ubo.Unbounded_String;
      root_json           : json.JSON_Value;

   begin
      --  check if a date was specified and if the file exists
      file_exists : Boolean := False;
      if date_index > 0 then
         --  construct the pathname
         hashed_file_name :=
           ubo.To_Unbounded_String
             (sys_conf.Cache_Dir.To_String
              & os.Directory_Separator
              & md5.Digest (constructed_url)
              & ".json");
         io.Put_Line ("checking for " & hashed_file_name.To_String);

         --  create the directory if it does not exist
         if not dir.Exists (sys_conf.Cache_Dir.To_String) then
            dir.Create_Directory (sys_conf.Cache_Dir.To_String);
         end if;

         --  check if the file exists
         file_exists := dir.Exists (hashed_file_name.To_String);
      end if;

      if file_exists then
         --  if it exists then read the root json from cache
         res : constant json.Read_Result :=
           json.Read_File (hashed_file_name.To_String);
         if res.Success then
            root_json := res.Value;
            io.Put_Line ("cache hit.");
         else
            io.Put_Line (res.Error'Image);
            raise Program_Error;
         end if;
      else
         --  read from Oanda
         io.Put_Line (constructed_url);
         root_json :=
           json.Read
             (Fetch_Candle_Data (oanda.Token.To_String, constructed_url));

         --  if a date was specified then cache the result
         if date_index > 0 then
            file_handle : io.File_Type;
            io.Create
              (File => file_handle, Name => hashed_file_name.To_String);
            io.Put (File => file_handle, Item => json.Write (root_json));
            io.Close (file_handle);
         end if;
      end if;

      --  get the candles from the root json blob
      unmapped_json_array := root_json.Get ("candles");
      io.Put_Line
        ("candles retrieved:" & json.Length (unmapped_json_array)'Image);

      --  map the candles from the raw json array to internal rep
      for i in 1 .. count loop
         out_candles (i) :=
           Make_Candle (json.Array_Element (unmapped_json_array, i));
      end loop;

      return out_candles;

   end Fetch_Candles;

end Oanda_Exchange;
