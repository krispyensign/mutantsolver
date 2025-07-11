pragma Ada_2022;

with Config;
with TOML;
with TOML.File_IO;
with Oanda_Exchange;
with TA;
with Core;
with Pools;
with Common; use Common;
with Ada.Text_IO;
with Ada.Real_Time;

procedure Mutantsolver is
   package io renames Ada.Text_IO;

   --  load the configs from the toml files
   result     : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda      : constant Config.Oanda_Access := Config.Load_Oanda (result);
   chart      : constant Config.Chart_Config :=
     Config.Load_Chart_Config (result);
   count      : constant Positive :=
     (chart.Offline_Set_Size + chart.Online_Set_Size);
   ex_candles : Core.Candles (1 .. count);
   --  initialize the Technical analysis library TA-Lib
   ta_result  : constant Integer := TA.TA_Initialize;

   --  full_data_pool is a pool that contains all candles
   package full_p is new Pools (Count => count);
   full_data_pool : full_p.Pool;

   --  offline_data_pool is a pool that contains the offline candles that will
   --  be used to solve for optimized entry, exit, source, take profit, and
   --  stop loss components
   package offline_p is new Pools (Count => chart.Offline_Set_Size);
   offline_data_pool : offline_p.Pool;

   --  tp_sl_offline_data_pool is a pool that contains a copied slice of the
   --  offline candles that will be used for solving for optimized take profit
   --  and stop loss multipliers
   package tp_sl_offline_p is new
     Pools (Count => chart.TP_SL_Offline_Set_Size);
   tp_sl_offline_data_pool : tp_sl_offline_p.Pool;

   --  online_data_pool is a pool that contains the online candles that
   --  will not be solved for and are only used for zero knowledge evaluation
   --  of the selected strategy
   package online_p is new Pools (Count => chart.Online_Set_Size);
   online_data_pool        : online_p.Pool;

   current_scenario        : Core.Scenario;
   current_scenario_result : Core.Scenario_Result (1 .. count);
   best_scenario           : Core.Scenario;
   best_scenario_result    : Core.Scenario_Result (1 .. count);

   one_true_count : Integer := 0;
   start_time : Ada.Real_Time.Time;
   end_time : Ada.Real_Time.Time;
   total_time_duration : Ada.Real_Time.Time_Span;

begin
   --  fetch the candles
   ex_candles := Oanda_Exchange.Fetch_Candles (oanda, chart);

   --  populate the full data pool from the retrieved candles
   full_data_pool := full_p.Make_Pool (ex_candles, chart.Time_Period_Interval);

   --  partition the offline data pool
   offline_data_pool :=
     [for i in Common.Pool_Key'Range
      => offline_p.Swim_Lane
           (full_data_pool (i) (1 .. chart.Offline_Set_Size))];
   --  populate the tp/sl offline data pool with a segment of the main
   --  offline pool
   tp_sl_offline_data_pool :=
     [for i in Common.Pool_Key'Range
      => tp_sl_offline_p.Swim_Lane
           (offline_data_pool (i)
              (chart.Offline_Set_Size - chart.TP_SL_Offline_Set_Size + 1
               .. chart.Offline_Set_Size))];

   --  poplate the simulated online data pool for zero knowledge tests
   online_data_pool :=
     [for i in Common.Pool_Key'Range
      => online_p.Swim_Lane
           (full_data_pool (i) (chart.Offline_Set_Size + 1 .. count))];

   current_scenario :=
     (Is_Quasi               => False,
      Take_Profit_Multiplier => 0.0,
      Stop_Loss_Multiplier   => 0.0,
      Entry_Key              => Common.Ask_Open,
      Exit_Key               => Common.Ask_Open,
      WMA_Source_Key         => Common.WMA_Ask_Open,
      Num_Digits             => chart.Num_Digits);

   start_time := Ada.Real_Time.Clock;
   for entry_key in Common.Candle_Key'Range loop
      current_scenario.Entry_Key := entry_key;
      for exit_key in Common.Candle_Key'Range loop
         current_scenario.Exit_Key := exit_key;
         for wma_source_key in Common.WMA_Source_Key'Range loop
            current_scenario.WMA_Source_Key := wma_source_key;
            one_true_count := one_true_count + 1;

            for i in chart.Time_Period_Interval .. chart.Offline_Set_Size loop
               offline_p.Kernel
                 (offline_data_pool,
                  i,
                  current_scenario,
                  current_scenario_result);
            end loop;

            --  check best versus current
         end loop;
      end loop;
   end loop;
   end_time := Ada.Real_Time.Clock;
   total_time_duration := Ada.Real_Time."-"(end_time, start_time);

   io.Put_Line (one_true_count'Image);
   io.Put_Line (total_time_duration'Image);

end Mutantsolver;
