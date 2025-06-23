pragma Ada_2022;

with Config;
with TOML;
with TOML.File_IO;
with Core;
with Oanda_Exchange;
with TA;
with Ada.Text_IO;
with common;

procedure Mutantsolver is
   package io renames Ada.Text_IO;

   result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda  : constant Config.Oanda_Access := Config.Load_Oanda (result);
   chart  : constant Config.Chart_Config := Config.Load_Chart_Config (result);
   count  : constant Integer := (chart.Train_Set_Size + chart.Sample_Set_Size);

   fetched_candles : Core.Candles (1 .. count);

   ta_result : Integer := TA.TA_Initialize;
   calc_result  : common.Real_Array (1 .. count);

begin
   fetched_candles := Oanda_Exchange.Fetch_Candles (oanda, chart);
   fetched_candles (1) :=
     Core.Make_HA_Candle (fetched_candles (1), fetched_candles (1));
   for i in 2 .. count loop
      fetched_candles (i) :=
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
      out_real    => calc_result);

   io.Put_Line (calc_result (calc_result'Last)'Image);

   for i in 1 .. count loop
      fetched_candles (i).ATR := calc_result (i);
   end loop;

  TA.Calc_TA_WMA(
     in_real => [for i in fetched_candles'Range => fetched_candles (i).Ask_Open],
     time_period => 20,
     out_real => calc_result);
  for i in 1 .. count loop
     fetched_candles (i).WMA_Ask_Open := calc_result (i);
  end loop;


end Mutantsolver;
