pragma Ada_2022;

with Config;
with TOML;
with TOML.File_IO;
with Oanda_Exchange;
with Core;
with Pools;
with Common; use Common;
with Ada.Text_IO;
with Ada.Real_Time;
with Kernel;
with Kernel_Ops;

procedure Mutantsolver is
   package io renames Ada.Text_IO;
   --  load the configs from the toml files
   result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda  : constant Config.Oanda_Access := Config.Load_Oanda (result);
   chart  : constant Config.Chart_Config := Config.Load_Chart_Config (result);
   count  : constant Positive :=
     (chart.Offline_Set_Size + chart.Online_Set_Size);

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
   throughput          : Float := 0.0;

   --  scenario reporting variables
   offline_results                    :
     Kernel.Kernel_Elements (1 .. chart.Offline_Set_Size);
   zk_online_results                  :
     Kernel.Kernel_Elements (1 .. chart.Online_Set_Size);
   refined_zk_online_results          :
     Kernel.Kernel_Elements (1 .. chart.Online_Set_Size);
   pk_online_results                  :
     Kernel.Kernel_Elements (1 .. chart.Online_Set_Size);
   find_max_offline_result            : Kernel_Ops.Operation_Result;
   find_refined_zk_offline_max_result : Kernel_Ops.Operation_Result;
   find_pk_online_max_result          : Kernel_Ops.Operation_Result;

   pragma Assert (offline_results'Length = chart.Offline_Set_Size);
   pragma Assert (offline_data_pool'Length = chart.Offline_Set_Size);
   pragma Assert (online_data_pool'Length = chart.Online_Set_Size);
   pragma
     Assert (tp_sl_offline_data_pool'Length = chart.TP_SL_Offline_Set_Size);
   pragma Assert (zk_online_results'Length = chart.Online_Set_Size);
   pragma Assert (refined_zk_online_results'Length = chart.Online_Set_Size);
   pragma Assert (pk_online_results'Length = chart.Online_Set_Size);

   procedure Print_Most_Recent_Candle is
   begin
      for k in Common.Pool_Key'Range loop
         io.Put_Line
           (k'Image
            & " =>"
            & online_data_pool (chart.Online_Set_Size - 1) (k)'Image);
      end loop;
   end Print_Most_Recent_Candle;

   function Recover_Results
     (p : Common.Row_Pool; conf : Kernel.Scenario_Config; count : Positive)
      return Kernel.Kernel_Elements
   is
      results : Kernel.Kernel_Elements (1 .. count);
   begin
      for i in chart.Time_Period_Interval .. count loop
         Kernel.Kernel
           (curr      => p (i),
            prev      => p (i - 1),
            prev_prev => p (i - 2),
            conf      => conf,
            index     => i,
            results   => results);
      end loop;

      return results;
   end Recover_Results;

   procedure Summarize_Results is
   begin
      --  print the configs
      io.Put_Line ("");
      io.Put_Line ("Offline result config");
      io.Put_Line (find_max_offline_result.best_scenario_report'Image);
      io.Put_Line ("");
      io.Put_Line ("Refined result config");
      io.Put_Line
        (find_refined_zk_offline_max_result.best_scenario_report'Image);

      --  print the totals
      io.Put_Line
        ("et total:"
         & offline_results (chart.Offline_Set_Size).Exit_Total'Image);
      io.Put_Line
        ("zk total:"
         & zk_online_results (chart.Online_Set_Size).Exit_Total'Image);
      io.Put_Line
        ("refined zk total:"
         & refined_zk_online_results (chart.Online_Set_Size).Exit_Total'Image);
      io.Put_Line
        ("pk total:"
         & pk_online_results (chart.Online_Set_Size).Exit_Total'Image);
      io.Put_Line
        ("found offline:"
         & find_max_offline_result.total_found'Image
         & " /"
         & find_max_offline_result.total_count'Image);

      --  print performance metrics
      io.Put_Line ("time:" & total_time_duration'Image & "s");
      throughput :=
        Float (find_max_offline_result.total_count)
        / Float (Ada.Real_Time.To_Duration (total_time_duration));
      io.Put_Line ("throughput:" & throughput'Image);

   end Summarize_Results;

begin
   start_time := Ada.Real_Time.Clock;

   --  find the offline max
   find_max_offline_result :=
     Kernel_Ops.Find_Max (p => offline_data_pool, chart => chart);

   --  find the refined offline max
   find_refined_zk_offline_max_result :=
     Kernel_Ops.Find_Max_TP_SL
       (p     => tp_sl_offline_data_pool,
        chart => chart,
        conf  => find_max_offline_result.best_scenario_report.Config);

   --  find the perfect knowledge max
   find_pk_online_max_result :=
     Kernel_Ops.Find_Max_TP_SL
       (p     => online_data_pool,
        chart => chart,
        conf  => find_max_offline_result.best_scenario_report.Config);

   --  recover the offline results
   offline_results :=
     Recover_Results
       (p     => offline_data_pool,
        conf  => find_max_offline_result.best_scenario_report.Config,
        count => chart.Offline_Set_Size);

   --  recover the online zero knowledge results
   zk_online_results :=
     Recover_Results
       (p     => online_data_pool,
        conf  => find_max_offline_result.best_scenario_report.Config,
        count => chart.Online_Set_Size);

   --  recover the online zero knowledge refined results
   refined_zk_online_results :=
     Recover_Results
       (p     => online_data_pool,
        conf  =>
          find_refined_zk_offline_max_result.best_scenario_report.Config,
        count => chart.Online_Set_Size);

   --  recover the online perfect knowledge results
   pk_online_results :=
     Recover_Results
       (p     => online_data_pool,
        conf  => find_pk_online_max_result.best_scenario_report.Config,
        count => chart.Online_Set_Size);

   end_time := Ada.Real_Time.Clock;
   total_time_duration := Ada.Real_Time."-" (end_time, start_time);

   Print_Most_Recent_Candle;
   Summarize_Results;

end Mutantsolver;
