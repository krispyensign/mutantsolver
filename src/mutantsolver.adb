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

   --  fetch the candles and allocate candles on the stack
   fetched_candles : constant Core.Candles (1 .. count) :=
     Oanda_Exchange.Fetch_Candles (oanda, chart);

   --  apply the heiken ashi transform
   ha_candles : constant Core.HA_Candles (1 .. count) :=
     [for i in fetched_candles'Range
      => (if i = 1
          then Core.Make_HA_Candle (fetched_candles (1), fetched_candles (1))
          else
            Core.Make_HA_Candle
              (fetched_candles (i), fetched_candles (i - 1)))];

   --  initialize the Technical analysis library TA-Lib
   ta_result : Integer := TA.TA_Initialize;

   --  allocate the full data pool on the heap and apply the retrieved candles
   full_data_pool : constant Core.Pool_T (Core.Column_Key, 1 .. count) :=
     [Core.Ask_Open =>
          [for i in 1 .. count => fetched_candles (i).Ask_Open],
      Core.Ask_High     =>
          [for i in 1 .. count => fetched_candles (i).Ask_High],
      Core.Ask_Low      =>
          [for i in 1 .. count => fetched_candles (i).Ask_Low],
      Core.Ask_Close    =>
          [for i in 1 .. count => fetched_candles (i).Ask_Close],
      Core.Mid_Open     =>
          [for i in 1 .. count => fetched_candles (i).Mid_Open],
      Core.Mid_High     =>
          [for i in 1 .. count => fetched_candles (i).Mid_High],
      Core.Mid_Low      =>
          [for i in 1 .. count => fetched_candles (i).Mid_Low],
      Core.Mid_Close    =>
          [for i in 1 .. count => fetched_candles (i).Mid_Close],
      Core.Bid_Open     =>
          [for i in 1 .. count => fetched_candles (i).Bid_Open],
      Core.Bid_High     =>
          [for i in 1 .. count => fetched_candles (i).Bid_High],
      Core.Bid_Low      =>
          [for i in 1 .. count => fetched_candles (i).Bid_Low],
      Core.Bid_Close    =>
          [for i in 1 .. count => fetched_candles (i).Bid_Close],
      Core.HA_Ask_Open  =>
        [for i in 1 .. count => ha_candles (i).Ask_Open],
      Core.HA_Ask_High  =>
        [for i in 1 .. count => ha_candles (i).Ask_High],
      Core.HA_Ask_Low   =>
        [for i in 1 .. count => ha_candles (i).Ask_Low],
      Core.HA_Ask_Close =>
        [for i in 1 .. count => ha_candles (i).Ask_Close],
      Core.HA_Mid_Open  =>
        [for i in 1 .. count => ha_candles (i).Mid_Open],
      Core.HA_Mid_High  =>
        [for i in 1 .. count => ha_candles (i).Mid_High],
      Core.HA_Mid_Low   =>
        [for i in 1 .. count => ha_candles (i).Mid_Low],
      Core.HA_Mid_Close =>
        [for i in 1 .. count => ha_candles (i).Mid_Close],
      Core.HA_Bid_Open  =>
        [for i in 1 .. count => ha_candles (i).Bid_Open],
      Core.HA_Bid_High  =>
        [for i in 1 .. count => ha_candles (i).Bid_High],
      Core.HA_Bid_Low   =>
        [for i in 1 .. count => ha_candles (i).Bid_Low],
      Core.HA_Bid_Close =>
        [for i in 1 .. count => ha_candles (i).Bid_Close],
      others => [for i in 1 .. count => 0.0]];

begin
   --  Calculate the ATR
   TA.Calc_TA_ATR
     (in_high     => full_data_pool (Core.Mid_High) (1 .. count),
      in_low      => full_data_pool (Core.Mid_Low),
      in_close    => full_data_pool (Core.Mid_Close),
      time_period => chart.Time_Period_Interval,
      out_real    => Core.Real_Array_Ptr(full_data_pool (Core.ATR)));

   --  TA.Calc_TA_WMA
   --    (in_real     =>
   --       [for i in fetched_candles'Range => fetched_candles (i).Ask_Open],
   --     time_period => chart.Time_Period_Interval,
   --     out_real    => calc_result);
   --  for i in 1 .. count loop
   --     fetched_candles (i).WMA_Ask_Open := calc_result (i);
   --  end loop;

end Mutantsolver;
