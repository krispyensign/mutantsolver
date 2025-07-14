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
with Kernel;

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
   --  initialize the Technical analysis library TA-Lib
   ta_result  : constant Integer := TA.TA_Initialize;
   --  fetch the candles
   ex_candles : constant Core.Candles (1 .. count) :=
     Oanda_Exchange.Fetch_Candles (oanda, chart);

   --  full_data_pool is a pool that contains all candles
   --  populate the full data pool from the retrieved candles
   package full_p is new Pools (Count => count);
   full_data_pool : constant full_p.Pool :=
     full_p.Make_Pool (ex_candles, chart.Time_Period_Interval);

   --  offline_data_pool is a pool that contains the offline candles that will
   --  be used to solve for optimized entry, exit, source, take profit, and
   --  stop loss components
   --  partition the offline data pool
   package offline_p is new Pools (Count => chart.Offline_Set_Size);
   offline_data_pool : constant Common.Row_Pool :=
     offline_p.Make_Row_Pool
       ([for i in Common.Pool_Key'Range
         => offline_p.Swim_Lane
              (full_data_pool (i) (1 .. chart.Offline_Set_Size))]);

   --  tp_sl_offline_data_pool is a pool that contains a copied slice of the
   --  offline candles that will be used for solving for optimized take profit
   --  and stop loss multipliers
   --  populate the tp/sl offline data pool with a segment of the main
   --  offline pool
   package tp_sl_offline_p is new
     Pools (Count => chart.TP_SL_Offline_Set_Size);
   tp_sl_offline_data_pool : constant Common.Row_Pool :=
     tp_sl_offline_p.Make_Row_Pool
       ([for key in Common.Pool_Key'Range
         => tp_sl_offline_p.Swim_Lane
              (full_data_pool (key)
                 (chart.Offline_Set_Size - chart.TP_SL_Offline_Set_Size + 1
                  .. chart.Offline_Set_Size))]);

   --  online_data_pool is a pool that contains the online candles that
   --  will not be solved for and are only used for zero knowledge evaluation
   --  of the selected strategy
   --  poplate the simulated online data pool for zero knowledge tests
   package online_p is new Pools (Count => chart.Online_Set_Size);
   online_data_pool : constant Common.Row_Pool :=
     online_p.Make_Row_Pool
       ([for i in Common.Pool_Key'Range
         => online_p.Swim_Lane
              (full_data_pool (i) (chart.Offline_Set_Size + 1 .. count))]);

   --  performance metrics
   start_time          : Ada.Real_Time.Time;
   end_time            : Ada.Real_Time.Time;
   total_time_duration : Ada.Real_Time.Time_Span;

   --  multithreading
   num_tasks    : constant Positive := 4;
   kernel_tasks :
     array (Positive range 1 .. num_tasks) of Kernel.Process_Kernel;

   --  scenario reporting variables
   best_scenario_report : Kernel.Scenario_Report;
   one_true_count       : Natural := 0;
   total_found          : Natural := 0;

begin
   start_time := Ada.Real_Time.Clock;
   for entry_key in Common.Candle_Key'Range loop
      for exit_key in Common.Candle_Key'Range loop
         for wma_source_key in Common.WMA_Source_Key'Range loop
            one_true_count := one_true_count + 1;
            kernel_tasks (1 + (one_true_count mod num_tasks)).Start
              (p    => offline_data_pool,
               conf =>
                 (Start_Index            => chart.Time_Period_Interval,
                  Entry_Key              => entry_key,
                  Exit_Key               => exit_key,
                  WMA_Source_Key         => wma_source_key,
                  Take_Profit_Multiplier => 0.0,
                  Stop_Loss_Multiplier   => 0.0,
                  Num_Digits             => chart.Num_Digits,
                  Is_Quasi               => False));
         end loop;
      end loop;
   end loop;
   end_time := Ada.Real_Time.Clock;
   total_time_duration := Ada.Real_Time."-" (end_time, start_time);

   io.Put_Line (one_true_count'Image);
   io.Put_Line (total_found'Image);
   io.Put_Line (total_time_duration'Image);

end Mutantsolver;
