with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Util.Dates.ISO8601;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
with Ada.Strings.Fixed;
with Ada.Calendar.Conversions;

package body candles is
   package ubo renames Ada.Strings.Unbounded;
   package fixed renames Ada.Strings.Fixed;
   package strings renames Ada.Strings;
   package io renames Ada.Text_IO;
   package conversions renames Ada.Calendar.Conversions;

   function Construct_URL
     (oanda : Oanda_Access; chart : Chart_Config) return String
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
     (oanda : Oanda_Access; chart : Chart_Config; from_time : calendar.Time)
      return String
   is
      count : constant Integer := chart.Sample_Set_Size + chart.Train_Set_Size;

      seconds : constant Long_Long_Integer :=
        Long_Long_Integer (conversions.To_Unix_Time_64 (from_time));

      constructed_url : constant String :=
        ubo.To_String (oanda.URL)
        & "/v3/instruments/"
        & chart.Instrument
        & "/candles?price=MAB&granularity="
        & chart.Granularity
        & "&count="
        & fixed.Trim (count'Image, strings.Both)
        & "&from="
        & fixed.Trim (seconds'Image, strings.Both);
   begin
      return constructed_url;
   end Construct_URL;

   function Make_Candle
     (current_candle : json.JSON_Value; previous_candle : Candle) return Candle
   is
      Ask_Open  : constant Float :=
        Float'Value (current_candle.Get ("ask").Get ("o"));
      Ask_High  : constant Float :=
        Float'Value (current_candle.Get ("ask").Get ("h"));
      Ask_Low   : constant Float :=
        Float'Value (current_candle.Get ("ask").Get ("l"));
      Ask_Close : constant Float :=
        Float'Value (current_candle.Get ("ask").Get ("c"));
      Mid_Open  : constant Float :=
        Float'Value (current_candle.Get ("mid").Get ("o"));
      Mid_High  : constant Float :=
        Float'Value (current_candle.Get ("mid").Get ("h"));
      Mid_Low   : constant Float :=
        Float'Value (current_candle.Get ("mid").Get ("l"));
      Mid_Close : constant Float :=
        Float'Value (current_candle.Get ("mid").Get ("c"));
      Bid_Open  : constant Float :=
        Float'Value (current_candle.Get ("bid").Get ("o"));
      Bid_High  : constant Float :=
        Float'Value (current_candle.Get ("bid").Get ("h"));
      Bid_Low   : constant Float :=
        Float'Value (current_candle.Get ("bid").Get ("l"));
      Bid_Close : constant Float :=
        Float'Value (current_candle.Get ("bid").Get ("c"));
   begin
      return
        (Volume       => current_candle.Get ("volume"),
         Complete     => current_candle.Get ("complete"),
         Ask_Open     => Ask_Open,
         Ask_High     => Ask_High,
         Ask_Low      => Ask_Low,
         Ask_Close    => Ask_Close,
         Mid_Open     => Mid_Open,
         Mid_High     => Mid_High,
         Mid_Low      => Mid_Low,
         Mid_Close    => Mid_Close,
         Bid_Open     => Bid_Open,
         Bid_High     => Bid_High,
         Bid_Low      => Bid_Low,
         Bid_Close    => Bid_Close,
         HA_Ask_Open  =>
           (previous_candle.Ask_Open + previous_candle.Ask_Close) / 2.0,
         HA_Ask_High  => Float'Max (Float'Max (Ask_Open, Ask_Close), Ask_High),
         HA_Ask_Low   => Float'Max (Float'Max (Ask_Open, Ask_Close), Ask_Low),
         HA_Ask_Close => (Ask_Open + Ask_High + Ask_Low + Ask_Close) / 4.0,
         HA_Mid_Open  =>
           (previous_candle.Mid_Open + previous_candle.Mid_Close) / 2.0,
         HA_Mid_High  => Float'Max (Float'Max (Mid_Open, Mid_Close), Mid_High),
         HA_Mid_Low   => Float'Max (Float'Max (Mid_Open, Mid_Close), Mid_Low),
         HA_Mid_Close => (Mid_Open + Mid_High + Mid_Low + Mid_Close) / 4.0,
         HA_Bid_Open  =>
           (previous_candle.Bid_Open + previous_candle.Bid_Close) / 2.0,
         HA_Bid_High  => Float'Max (Float'Max (Bid_Open, Bid_Close), Bid_High),
         HA_Bid_Low   => Float'Max (Float'Max (Bid_Open, Bid_Close), Bid_Low),
         HA_Bid_Close => (Bid_Open + Bid_High + Bid_Low + Bid_Close) / 4.0,
         Time         =>
           Util.Dates.ISO8601.Value (current_candle.Get ("time")));

   end Make_Candle;

   function Make_Candle (current_candle : json.JSON_Value) return Candle is
      Ask_Open  : constant Float :=
        Float'Value (current_candle.Get ("ask").Get ("o"));
      Ask_High  : constant Float :=
        Float'Value (current_candle.Get ("ask").Get ("h"));
      Ask_Low   : constant Float :=
        Float'Value (current_candle.Get ("ask").Get ("l"));
      Ask_Close : constant Float :=
        Float'Value (current_candle.Get ("ask").Get ("c"));
      Mid_Open  : constant Float :=
        Float'Value (current_candle.Get ("mid").Get ("o"));
      Mid_High  : constant Float :=
        Float'Value (current_candle.Get ("mid").Get ("h"));
      Mid_Low   : constant Float :=
        Float'Value (current_candle.Get ("mid").Get ("l"));
      Mid_Close : constant Float :=
        Float'Value (current_candle.Get ("mid").Get ("c"));
      Bid_Open  : constant Float :=
        Float'Value (current_candle.Get ("bid").Get ("o"));
      Bid_High  : constant Float :=
        Float'Value (current_candle.Get ("bid").Get ("h"));
      Bid_Low   : constant Float :=
        Float'Value (current_candle.Get ("bid").Get ("l"));
      Bid_Close : constant Float :=
        Float'Value (current_candle.Get ("bid").Get ("c"));
   begin
      return
        (Volume       => current_candle.Get ("volume"),
         Complete     => current_candle.Get ("complete"),
         Ask_Open     => Ask_Open,
         Ask_High     => Ask_High,
         Ask_Low      => Ask_Low,
         Ask_Close    => Ask_Close,
         Mid_Open     => Mid_Open,
         Mid_High     => Mid_High,
         Mid_Low      => Mid_Low,
         Mid_Close    => Mid_Close,
         Bid_Open     => Bid_Open,
         Bid_High     => Bid_High,
         Bid_Low      => Bid_Low,
         Bid_Close    => Bid_Close,
         HA_Ask_Open  => (Ask_Open + Ask_Close) / 2.0,
         HA_Ask_High  => Float'Max (Float'Max (Ask_Open, Ask_Close), Ask_High),
         HA_Ask_Low   => Float'Max (Float'Max (Ask_Open, Ask_Close), Ask_Low),
         HA_Ask_Close => (Ask_Open + Ask_High + Ask_Low + Ask_Close) / 4.0,
         HA_Mid_Open  => (Mid_Open + Mid_Close) / 2.0,
         HA_Mid_High  => Float'Max (Float'Max (Mid_Open, Mid_Close), Mid_High),
         HA_Mid_Low   => Float'Max (Float'Max (Mid_Open, Mid_Close), Mid_Low),
         HA_Mid_Close => (Mid_Open + Mid_High + Mid_Low + Mid_Close) / 4.0,
         HA_Bid_Open  => (Bid_Open + Bid_Close) / 2.0,
         HA_Bid_High  => Float'Max (Float'Max (Bid_Open, Bid_Close), Bid_High),
         HA_Bid_Low   => Float'Max (Float'Max (Bid_Open, Bid_Close), Bid_Low),
         HA_Bid_Close => (Bid_Open + Bid_High + Bid_Low + Bid_Close) / 4.0,
         Time         =>
           Util.Dates.ISO8601.Value (current_candle.Get ("time")));

   end Make_Candle;

   function Fetch_Candles
     (oanda : Oanda_Access; chart : Chart_Config) return Candles_Frame
   is
      unmapped_json_array : json.JSON_Array;
      count               : constant Integer :=
        chart.Sample_Set_Size + chart.Train_Set_Size;
      out_candles         : Candles_Frame (0 .. count);

   begin
      --  fetch the candles
      Util.Http.Clients.Curl.Register;
      declare
         http            : Util.Http.Clients.Client;
         response        : Util.Http.Clients.Response;
         constructed_url : constant String := Construct_URL (oanda, chart);

      begin
         --  setup headers
         http.Add_Header ("Content-Type", "application/json");
         http.Add_Header ("Bearer", ubo.To_String (oanda.Token));
         http.Get (constructed_url, response);
         if response.Get_Status /= 200 then
            io.Put_Line (response.Get_Body);
            io.Put_Line (constructed_url);
            io.Put_Line ("error retrieving candles");
            raise Program_Error;
         end if;

         --  print to screen for now what the URL should look like
         io.Put_Line (response.Get_Body);
         io.Put_Line (constructed_url);
         unmapped_json_array := json.Read (response.Get_Body).Get ("candles");
      end;

      out_candles (1) :=
        Make_Candle (json.Array_Element (unmapped_json_array, 1));
      for i in 2 .. count loop
         out_candles (i) :=
           Make_Candle (json.Array_Element (unmapped_json_array, i));
      end loop;

      return out_candles;

   end Fetch_Candles;

end candles;
