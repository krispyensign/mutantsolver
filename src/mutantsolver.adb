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
       ([for i in Common.Pool_Key'Range =>
           offline_p.Swim_Lane
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
       ([for key in Common.Pool_Key'Range =>
           tp_sl_offline_p.Swim_Lane
             (full_data_pool (key)
                (chart.Offline_Set_Size
                 - chart.TP_SL_Offline_Set_Size
                 + 1
                 .. chart.Offline_Set_Size))]);

   --  online_data_pool is a pool that contains the online candles that
   --  will not be solved for and are only used for zero knowledge evaluation
   --  of the selected strategy
   --  poplate the simulated online data pool for zero knowledge tests
   package online_p is new Pools (Count => chart.Online_Set_Size);
   online_data_pool : constant Common.Row_Pool :=
     online_p.Make_Row_Pool
       ([for i in Common.Pool_Key'Range =>
           online_p.Swim_Lane
             (full_data_pool (i) (chart.Offline_Set_Size + 1 .. count))]);

   --  performance metrics
   start_time          : Ada.Real_Time.Time;
   end_time            : Ada.Real_Time.Time;
   total_time_duration : Ada.Real_Time.Time_Span;

   --  multithreading
   num_tasks    : constant Positive := 1;
   kernel_tasks :
     array (Positive range 1 .. num_tasks) of Kernel.Process_Kernel;

   --  scenario reporting variables
   best_scenario_report : Kernel.Scenario_Report;
   one_true_count       : Natural := 0;
   total_found          : Natural := 0;
   temp_total_found     : Natural := 0;

begin
   for k in Common.Pool_Key'Range loop
      io.Put_Line
        (k'Image
         & " => "
         & online_data_pool (chart.Online_Set_Size - 1) (k)'Image);
   end loop;

   io.Put_Line (offline_data_pool (1)'Length'Image);
   io.Put_Line (offline_data_pool'Length'Image);
   start_time := Ada.Real_Time.Clock;

   --  queue the configs to the solver tasks
   for wma_source_key in Common.WMA_Source_Key'Range loop
      for entry_key in Common.Candle_Key'Range loop
         for exit_key in Common.Candle_Key'Range loop
            for take_profit_multiplier of Common.Take_Profit_Multipliers loop
               for stop_loss_multiplier of Common.Stop_Loss_Multipliers loop
                  one_true_count := one_true_count + 1;
                  if one_true_count mod 100000 = 0 then
                     io.Put_Line (one_true_count'Image);
                  end if;
                  kernel_tasks (1 + (one_true_count mod num_tasks)).Start
                    (p    => offline_data_pool,
                     conf =>
                       (Start_Index            => chart.Time_Period_Interval,
                        Entry_Key              => entry_key,
                        Exit_Key               => exit_key,
                        WMA_Source_Key         => wma_source_key,
                        Take_Profit_Multiplier => take_profit_multiplier,
                        Stop_Loss_Multiplier   => stop_loss_multiplier,
                        Num_Digits             => chart.Num_Digits,
                        Is_Quasi               => False));
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;

   --  gather the reports and pick only the best
   if kernel_tasks'Length > 1 then
      for i in 2 .. kernel_tasks'Length loop
         declare
            temp_report : Kernel.Scenario_Report;
         begin
            kernel_tasks (i).Read (temp_report, temp_total_found);
            kernel_tasks (1).Update_Scenario (temp_report);
            total_found := total_found + temp_total_found;
         end;
      end loop;
   end if;
   end_time := Ada.Real_Time.Clock;
   total_time_duration := Ada.Real_Time."-" (end_time, start_time);

   kernel_tasks (1).Read (best_scenario_report, temp_total_found);
   total_found := temp_total_found;

   io.Put_Line ("found: " & total_found'Image & "/" & one_true_count'Image);

   kernel_tasks (1).Read (best_scenario_report, temp_total_found);
   io.Put_Line (best_scenario_report'Image);

   io.Put_Line ("time: " & total_time_duration'Image & "s");

end Mutantsolver;
