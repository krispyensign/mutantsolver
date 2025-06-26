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

   --  allocate the full data pool on the heap
   full_data_pool : constant Core.Pool_T (Core.Column_Key) :=
     [for i in Core.Column_Key'First .. Core.Column_Key'Last
      => new Core.Real_Array (1 .. count)];

begin
   --  apply the retrieved candles to the data pool
   full_data_pool (Core.Ask_Open) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Ask_Open];
   full_data_pool (Core.Ask_High) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Ask_High];
   full_data_pool (Core.Ask_Low) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Ask_Low];
   full_data_pool (Core.Ask_Close) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Ask_Close];
   full_data_pool (Core.Bid_Open) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Bid_Open];
   full_data_pool (Core.Bid_High) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Bid_High];
   full_data_pool (Core.Bid_Low) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Bid_Low];
   full_data_pool (Core.Bid_Close) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Bid_Close];
   full_data_pool (Core.Mid_Open) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Mid_Open];
   full_data_pool (Core.Mid_High) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Mid_High];
   full_data_pool (Core.Mid_Low) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Mid_Low];
   full_data_pool (Core.Mid_Close) (1 .. count) :=
     [for i in fetched_candles'Range => fetched_candles (i).Mid_Close];

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
