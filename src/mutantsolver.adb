pragma Ada_2022;

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Config; use Config;
with TOML;
with TOML.File_IO;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
with Util.Dates.ISO8601;
with Ada.Strings.Fixed;
with Ada.Calendar;
with GNATCOLL.JSON;

procedure Mutantsolver is
   package ubo renames Ada.Strings.Unbounded;
   package fixed renames Ada.Strings.Fixed;
   package strings renames Ada.Strings;
   package rt renames Ada.Calendar;
   package io renames Ada.Text_IO;
   package json renames GNATCOLL.JSON;

   result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda  : constant Oanda_Access := Load_Oanda (result);
   chart  : constant Chart_Config := Load_Chart_Config (result);

   --  construct URL to retrieve candles
   count : constant Integer := (chart.Train_Set_Size + chart.Sample_Set_Size);

   constructed_url : constant String :=
     ubo.To_String (oanda.URL)
     & "/v3/instruments/"
     & chart.Instrument
     & "/candles?price=MAB&granularity="
     & chart.Granularity
     & "&count="
     & fixed.Trim (count'Image, strings.Both);

   type Candle is record
      Complete  : Boolean;
      Open      : Float;
      High      : Float;
      Low       : Float;
      Close     : Float;
      Volume    : Integer;
      Time      : rt.Time;
   end record;

   unmapped_json_array : json.JSON_Array;

   ask_candles : array (1 .. count) of Candle;
   mid_candles : array (1 .. count) of Candle;
   bid_candles : array (1 .. count) of Candle;

begin
   --  setup provider
   Util.Http.Clients.Curl.Register;

   --  start block that should be a separate function
   declare
      http        : Util.Http.Clients.Client;
      response    : Util.Http.Clients.Response;
   begin
      --  setup headers
      http.Add_Header ("Content-Type", "application/json");
      http.Add_Header ("Bearer", ubo.To_String (oanda.Token));
      http.Get (constructed_url, response);
      if response.Get_Status /= 200 then
         io.Put_Line (response.Get_Body);
         io.Put_Line (constructed_url);
         io.Put_Line ("error retrieving candles");
         return;
      end if;

      --  print to screen for now what the URL should look like
      io.Put_Line (response.Get_Body);
      io.Put_Line (constructed_url);
      unmapped_json_array := json.Read (response.Get_Body).Get("candles");
      declare
		  current_candle : json.JSON_Value;
      begin
         for i in 1..count loop
			current_candle := json.Array_Element(unmapped_json_array, i);
            ask_candles (i) :=
              (Volume   => current_candle.Get ("volume"),
               Complete => current_candle.Get ("complete"),
			   Open => float'Value(current_candle.Get("ask").Get("o")),
			   High => Float'Value(current_candle.Get("ask").Get("h")),
			   Low => Float'Value(current_candle.Get("ask").Get("l")),
			   Close => Float'Value(current_candle.Get("ask").Get("c")),
			   Time => Util.Dates.ISO8601.Value(current_candle.Get("time")));

         end loop;
      end;
   end;


end Mutantsolver;
