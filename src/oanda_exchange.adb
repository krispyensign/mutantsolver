with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Util.Dates.ISO8601;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
with Ada.Strings.Fixed;
with Ada.Calendar.Conversions;

package body Oanda_Exchange is
   package ubo renames Ada.Strings.Unbounded;
   package fixed renames Ada.Strings.Fixed;
   package strings renames Ada.Strings;
   package io renames Ada.Text_IO;
   package conversions renames Ada.Calendar.Conversions;

   function Construct_URL
     (oanda : Config.Oanda_Access; chart : Config.Chart_Config) return String
   is
      count           : constant Integer :=
        chart.Sample_Set_Size + chart.Train_Set_Size;
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
     (oanda     : Config.Oanda_Access;
      chart     : Config.Chart_Config;
      from_time : calendar.Time) return String
   is
      seconds         : constant Long_Long_Integer :=
        Long_Long_Integer (conversions.To_Unix_Time_64 (from_time));
      constructed_url : constant String :=
        Construct_URL (oanda, chart)
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
     (token : String; constructed_url : String) return json.JSON_Array is
   begin
      --  fetch the candles
      Util.Http.Clients.Curl.Register;
      declare
         http     : Util.Http.Clients.Client;
         response : Util.Http.Clients.Response;
         candles  : json.JSON_Array;

      begin
         --  setup headers
         http.Add_Header ("Content-Type", "application/json");
         http.Add_Header ("Authorization", "Bearer " & token);
         --  io.Put_Line (token);
         http.Get (constructed_url, response);
         if response.Get_Status /= 200 then
            io.Put_Line (response.Get_Body);
            io.Put_Line (constructed_url);
            io.Put_Line ("error retrieving candles");
            raise Program_Error;
         end if;

         --  print to screen for now what the URL should look like
         --  io.Put_Line (response.Get_Body);
         io.Put_Line (constructed_url);
         candles := json.Read (response.Get_Body).Get ("candles");
         io.Put_Line ("candles retrieved: " & json.Length (candles)'Image);

         return candles;
      end;
   end Fetch_Candle_Data;

   function Fetch_Candles
     (oanda : Config.Oanda_Access; chart : Config.Chart_Config)
      return Core.Candles
   is
      unmapped_json_array : json.JSON_Array;
      count               : constant Integer :=
        chart.Sample_Set_Size + chart.Train_Set_Size;
      out_candles         : Core.Candles (1 .. count);
      constructed_url     : constant String := Construct_URL (oanda, chart);

   begin
      unmapped_json_array :=
        Fetch_Candle_Data (ubo.To_String (oanda.Token), constructed_url);
      for i in 1 .. count loop
         out_candles (i) :=
           Make_Candle (json.Array_Element (unmapped_json_array, i));
      end loop;

      return out_candles;

   end Fetch_Candles;

end Oanda_Exchange;
