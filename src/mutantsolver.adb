pragma Ada_2022;

with Config;
with TOML;
with TOML.File_IO;
with Core;
with Oanda_Exchange;
with TA;
with Ada.Text_IO;

procedure Mutantsolver is
   package io renames Ada.Text_IO;

   result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda  : constant Config.Oanda_Access := Config.Load_Oanda (result);
   chart  : constant Config.Chart_Config := Config.Load_Chart_Config (result);
   count  : constant Integer := (chart.Train_Set_Size + chart.Sample_Set_Size);

   fetched_candles : Core.Candles_Frame (1 .. count);
   ha_candles      : Core.HA_Candle_Frame (1 .. count);

   ta_result : Integer := TA.TA_Initialize;
   atr_calc  : TA.Real_Array (1 .. count);

begin
   fetched_candles := Oanda_Exchange.Fetch_Candles (oanda, chart);
   ha_candles (1) :=
     Core.Make_HA_Candle (fetched_candles (1), fetched_candles (1));
   for i in 2 .. count loop
      ha_candles (i) :=
        Core.Make_HA_Candle (fetched_candles (i), fetched_candles (i - 1));
   end loop;

   TA.Calc_TA_ATR
     (in_high     =>
        [for i in fetched_candles'Range => fetched_candles (i).Mid_High],
      in_low      =>
        [for i in fetched_candles'Range => fetched_candles (i).Mid_Low],
      in_close    =>
        [for i in fetched_candles'Range => fetched_candles (i).Mid_Close],
      time_period => 20,
      out_real    => atr_calc);
   io.Put_Line (atr_calc (atr_calc'Last)'Image);

end Mutantsolver;
