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
   atr_beg   : Integer;
   atr_end   : Integer;

begin
   fetched_candles := Oanda_Exchange.Fetch_Candles (oanda, chart);
   ha_candles (1) :=
     Core.Make_HA_Candle (fetched_candles (1), fetched_candles (1));
   for i in 2 .. count loop
      ha_candles (i) :=
        Core.Make_HA_Candle (fetched_candles (i), fetched_candles (i - 1));
   end loop;

   TA.Calc_TA_ATR
     (startIdx        => 0,
      endIdx          => count - 1,
      inHigh          =>
        [for i in fetched_candles'Range => fetched_candles (i).Mid_High],
      inLow           =>
        [for i in fetched_candles'Range => fetched_candles (i).Mid_Low],
      inClose         =>
        [for i in fetched_candles'Range => fetched_candles (i).Mid_Close],
      optInTimePeriod => 20,
      outBegIdx       => atr_beg,
      outNBElement    => atr_end,
      outReal         => atr_calc);
   io.Put_Line (atr_calc (atr_calc'Last)'Image);
   io.Put_Line (atr_beg'Image);
   io.Put_Line (atr_end'Image);

end Mutantsolver;
