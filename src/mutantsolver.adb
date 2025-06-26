pragma Ada_2022;

with Config;
with TOML;
with TOML.File_IO;
with Oanda_Exchange;
with TA;
with common;
with Core;

procedure Mutantsolver is
   --  load the configs from the toml files
   result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda  : constant Config.Oanda_Access := Config.Load_Oanda (result);
   chart  : constant Config.Chart_Config := Config.Load_Chart_Config (result);
   count  : constant Integer := (chart.Train_Set_Size + chart.Sample_Set_Size);

   --  allocate candles on the stack
   fetched_candles : Core.Candles (1 .. count);
   ha_candles      : Core.HA_Candles (1 .. count);

   --  initialize the Technical analysis library TA-Lib
   ta_result   : Integer := TA.TA_Initialize;

   --  allocate the full data pool on the stack
   full_data_pool : constant Core.Pool_T (Core.Column_Key) :=
     [for i in Core.Time .. Core.WMA_HA_Ask_Close
      => new Core.Real_Array (1 .. count)];

begin
   --  fetch the candles from oanda or some other data source
   fetched_candles := Oanda_Exchange.Fetch_Candles (oanda, chart);

   --  Generate the HA candles
   ha_candles (1) :=
     Core.Make_HA_Candle (fetched_candles (1), fetched_candles (1));
   for i in 2 .. count loop
      ha_candles (i) :=
        Core.Make_HA_Candle (fetched_candles (i), fetched_candles (i - 1));
   end loop;

   --  Calculate the ATR
   TA.Calc_TA_ATR
     (in_high     =>
        [for i in fetched_candles'Range => fetched_candles (i).Mid_High],
      in_low      =>
        [for i in fetched_candles'Range => fetched_candles (i).Mid_Low],
      in_close    =>
        [for i in fetched_candles'Range => fetched_candles (i).Mid_Close],
      time_period => chart.Time_Period_Interval,
      out_real    => full_data_pool (Core.ATR));

   --  TA.Calc_TA_WMA
   --    (in_real     =>
   --       [for i in fetched_candles'Range => fetched_candles (i).Ask_Open],
   --     time_period => chart.Time_Period_Interval,
   --     out_real    => calc_result);
   --  for i in 1 .. count loop
   --     fetched_candles (i).WMA_Ask_Open := calc_result (i);
   --  end loop;

end Mutantsolver;
