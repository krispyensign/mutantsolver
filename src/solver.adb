pragma Ada_2022;

with Ada.Text_IO;
with Ada.Real_Time;
with Pools;

package body Solver is
   package io renames Ada.Text_IO;

   function Is_In_Quasi_Keys (exit_key : Common.Candle_Key) return Boolean is
   begin
      case exit_key is
         when Common.Quasi_Keys =>
            return True;

         when others =>
            return False;
      end case;
   end Is_In_Quasi_Keys;

   function Is_Not_In_WMA_Quasi_Keys
     (wma_source_key : Common.WMA_Source_Key) return Boolean is
   begin
      case wma_source_key is
         when Common.WMA_Quasi_Keys =>
            return False;

         when others =>
            return True;
      end case;
   end Is_Not_In_WMA_Quasi_Keys;

   procedure Process_Kernel_Operation
     (p                      : Common.Row_Pool;
      result                 : in out Operation_Result;
      chart                  : Config.Chart_Config;
      entry_key              : Common.Candle_Key;
      exit_key               : Common.Candle_Key;
      wma_source_key         : Common.WMA_Source_Key;
      take_profit_multiplier : Float;
      stop_loss_multiplier   : Float)
   is
      cur_scen_res : Kernel.Kernel_Elements (1 .. p'Length);
      last_element : Kernel.Kernel_Element;
      ratio        : Float := 0.0;
      is_quasi     : Boolean := False;
      should_roll  : Boolean := False;
      conf         : Kernel.Scenario_Config;
      sr           : Kernel.Kernel_Element;

   begin
      --  prevent sl > tp
      --  TODO make this toggle
      --  if stop_loss_multiplier > take_profit_multiplier then
      --     return;
      --  end if;

      --  log progress
      result.Total_Count := result.Total_Count + 1;
      if result.Total_Count mod 100000 = 0 then
         io.Put_Line (result.Total_Count'Image);
      end if;

      --  determine quasi and if we should roll
      is_quasi := Is_In_Quasi_Keys (exit_key);
      if is_quasi then
         should_roll := Is_Not_In_WMA_Quasi_Keys (wma_source_key);
      end if;

      --  prepare the scenario config
      conf :=
        (Start_Index            => chart.Time_Period_Interval,
         Exit_Behavior          => chart.TPSL_Behavior,
         Should_Screen_ATR      => chart.Should_Screen_ATR,
         Is_Quasi               => is_quasi,
         Should_Roll            => should_roll,
         Num_Digits             => chart.Num_Digits,
         Take_Profit_Multiplier => take_profit_multiplier,
         Stop_Loss_Multiplier   => stop_loss_multiplier,
         Entry_Key              => entry_key,
         Exit_Key               => exit_key,
         WMA_Source_Key         => wma_source_key);

      --  process the kernel operation
      for i in conf.Start_Index .. p'Length loop
         Kernel.Kernel
           (curr      => p (i),
            prev      => p (i - 1),
            prev_prev => p (i - 2),
            index     => i,
            conf      => conf,
            results   => cur_scen_res);
      end loop;

      --  get the last element and prepare the scenario report
      last_element := cur_scen_res (p'Length - 1);
      result.Last_Scenario_Config := conf;
      result.Last_Scenario_Result := last_element;
      result.Total_Reported := result.Total_Reported + 1;

      --  if the final total is first then this is the first report
      if result.Best_Scenario_Result.Exit_Total = Long_Float'First then
         result.Best_Scenario_Result := sr;
         result.Best_Scenario_Config := conf;
         result.Best_Scenario_Ratio := 0.0;
      end if;

      --  if the final total is greater than zero then increment the found
      if sr.Exit_Total > 0.0 then
         result.Total_Found := result.Total_Found + 1;
      end if;

      if result.Best_Scenario_Result.Exit_Total <= 0.0
        and then sr.Exit_Total > result.Best_Scenario_Result.Exit_Total
      then
         --  calculate the ratio
         ratio :=
           (if sr.Wins + sr.Losses > 0
            then Float (sr.Wins) / Float (sr.Wins + sr.Losses)
            else 0.0);
         result.Best_Scenario_Ratio := ratio;
         result.Best_Scenario_Result := sr;
         result.Best_Scenario_Config := conf;
         io.Put_Line (result.Best_Scenario_Config'Image);
         io.Put_Line (result.Best_Scenario_Result'Image);
         io.Put_Line (result.Best_Scenario_Ratio'Image);
      end if;

      if sr.Exit_Total <= 0.0 then
         return;
      end if;

      if sr.Min_Exit_Total < 0.0
        and then sr.Max_Exit_Total < abs (sr.Min_Exit_Total)
      then
         return;
      end if;

      --  calculate the ratio
      ratio :=
        (if sr.Wins + sr.Losses > 0
         then Float (sr.Wins) / Float (sr.Wins + sr.Losses)
         else 0.0);

      --  if the ratio is greater than the best ratio
      --  and the final total is greater than the best final total
      --  then update the best scenario report
      if ratio > result.Best_Scenario_Ratio
        and then sr.Exit_Total > result.Best_Scenario_Result.Exit_Total
      then
         result.Best_Scenario_Result := sr;
         result.Best_Scenario_Ratio := ratio;
         result.Best_Scenario_Config := conf;
         io.Put_Line (result.Best_Scenario_Config'Image);
         io.Put_Line (result.Best_Scenario_Result'Image);
         io.Put_Line (result.Best_Scenario_Ratio'Image);
      end if;

   end Process_Kernel_Operation;

   function Find_Max
     (p : Common.Row_Pool; chart : Config.Chart_Config) return Operation_Result
   is
      temp_total_found : Natural := 0;
      result           : Operation_Result;
   begin
      --  queue the configs to the solver tasks
      result.Total_Found := 0;
      for wma_source_key in Common.WMA_Source_Key'Range loop
         for entry_key in Common.Candle_Key'Range loop
            for exit_key in Common.Candle_Key'Range loop
               for take_profit_multiplier of
                 Common.Offline_Take_Profit_Multipliers
               loop
                  for stop_loss_multiplier of
                    Common.Offline_Stop_Loss_Multipliers
                  loop
                     Process_Kernel_Operation
                       (p                      => p,
                        result                 => result,
                        chart                  => chart,
                        entry_key              => entry_key,
                        exit_key               => exit_key,
                        wma_source_key         => wma_source_key,
                        take_profit_multiplier => take_profit_multiplier,
                        stop_loss_multiplier   => stop_loss_multiplier);
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

      return result;

   end Find_Max;

   function Find_Max_TP_SL
     (p     : Common.Row_Pool;
      chart : Config.Chart_Config;
      conf  : Kernel.Scenario_Config) return Operation_Result
   is
      temp_total_found : Natural := 0;
      result           : Operation_Result;
   begin
      --  queue the configs to the solver tasks
      result.Total_Found := 0;
      for take_profit_multiplier of Common.Online_Take_Profit_Multipliers loop
         for stop_loss_multiplier of Common.Online_Stop_Loss_Multipliers loop
            Process_Kernel_Operation
              (p                      => p,
               result                 => result,
               chart                  => chart,
               entry_key              => conf.Entry_Key,
               exit_key               => conf.Exit_Key,
               wma_source_key         => conf.WMA_Source_Key,
               take_profit_multiplier => take_profit_multiplier,
               stop_loss_multiplier   => stop_loss_multiplier);
         end loop;
      end loop;

      return result;

   end Find_Max_TP_SL;

   procedure Print_Most_Recent_Candle
     (chart : Config.Chart_Config; p : Common.Row_Pool) is
   begin
      for k in Common.Pool_Key'Range loop
         io.Put_Line
           (k'Image & " =>" & p (chart.Online_Set_Size - 1) (k)'Image);
      end loop;
   end Print_Most_Recent_Candle;

   function Recover_Results
     (p     : Common.Row_Pool;
      conf  : Kernel.Scenario_Config;
      count : Positive;
      chart : Config.Chart_Config) return Kernel.Kernel_Elements
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

   procedure Offline_Solve
     (ex_candles : Core.Candles; chart : Config.Chart_Config; count : Positive)
   is
      --  full_data_pool is a pool that contains all candles
      --  populate the full data pool from the retrieved candles
      package full_p is new Pools (Count => count);
      full_data_pool : constant full_p.Pool :=
        full_p.Make_Pool (ex_candles, chart.Time_Period_Interval);

      --  offline_data_pool is a pool that contains the offline candles that
      --  will
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
      --  offline candles that will be used for solving for optimized take
      --  profit and stop loss multipliers
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
      --  will not be solved for and are only used for zero knowledge
      --  evaluation of the selected strategy
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
      find_max_offline_result            : Operation_Result;
      find_refined_zk_offline_max_result : Operation_Result;
      find_pk_online_max_result          : Operation_Result;

      pragma Assert (offline_results'Length = chart.Offline_Set_Size);
      pragma Assert (offline_data_pool'Length = chart.Offline_Set_Size);
      pragma Assert (online_data_pool'Length = chart.Online_Set_Size);
      pragma
        Assert (tp_sl_offline_data_pool'Length = chart.TP_SL_Offline_Set_Size);
      pragma Assert (zk_online_results'Length = chart.Online_Set_Size);
      pragma Assert (refined_zk_online_results'Length = chart.Online_Set_Size);
      pragma Assert (pk_online_results'Length = chart.Online_Set_Size);
   begin

      start_time := Ada.Real_Time.Clock;

      --  find the offline max
      find_max_offline_result :=
        Find_Max (p => offline_data_pool, chart => chart);

      --  find the refined offline max
      find_refined_zk_offline_max_result :=
        Find_Max_TP_SL
          (p     => tp_sl_offline_data_pool,
           chart => chart,
           conf  => find_max_offline_result.Best_Scenario_Config);

      --  find the perfect knowledge max
      find_pk_online_max_result :=
        Find_Max_TP_SL
          (p     => online_data_pool,
           chart => chart,
           conf  => find_max_offline_result.Best_Scenario_Config);

      --  recover the offline results
      offline_results :=
        Recover_Results
          (p     => offline_data_pool,
           chart => chart,
           conf  => find_max_offline_result.Best_Scenario_Config,
           count => chart.Offline_Set_Size);

      --  recover the online zero knowledge results
      zk_online_results :=
        Recover_Results
          (p     => online_data_pool,
           chart => chart,
           conf  => find_max_offline_result.Best_Scenario_Config,
           count => chart.Online_Set_Size);

      --  recover the online zero knowledge refined results
      refined_zk_online_results :=
        Recover_Results
          (p     => online_data_pool,
           chart => chart,
           conf  =>
             find_refined_zk_offline_max_result.Best_Scenario_Config,
           count => chart.Online_Set_Size);

      --  recover the online perfect knowledge results
      pk_online_results :=
        Recover_Results
          (p     => online_data_pool,
           chart => chart,
           conf  => find_pk_online_max_result.Best_Scenario_Config,
           count => chart.Online_Set_Size);

      end_time := Ada.Real_Time.Clock;
      total_time_duration := Ada.Real_Time."-" (end_time, start_time);

      Print_Most_Recent_Candle (chart, online_data_pool);
      --  print the configs
      io.Put_Line ("");
      io.Put_Line ("Offline result config");
      io.Put_Line (find_max_offline_result.Best_Scenario_Config'Image);
      io.Put_Line (find_max_offline_result.Best_Scenario_Result'Image);
      io.Put_Line (find_max_offline_result.Best_Scenario_Ratio'Image);
      io.Put_Line ("");
      io.Put_Line ("Refined result config");
      io.Put_Line
        (find_refined_zk_offline_max_result.Best_Scenario_Config'Image);
      io.Put_Line
        (find_refined_zk_offline_max_result.Best_Scenario_Result'Image);
      io.Put_Line
        (find_refined_zk_offline_max_result.Best_Scenario_Ratio'Image);

      --  print the totals
      io.Put_Line
        ("et total:"
         & offline_results (chart.Offline_Set_Size).Exit_Total'Image
         & " /"
         & offline_results (chart.Offline_Set_Size).Running_Total'Image);
      io.Put_Line
        ("zk total:"
         & zk_online_results (chart.Online_Set_Size).Exit_Total'Image
         & " /"
         & zk_online_results (chart.Online_Set_Size).Running_Total'Image);
      io.Put_Line
        ("refined zk total:"
         & refined_zk_online_results (chart.Online_Set_Size).Exit_Total'Image
         & " /"
         & refined_zk_online_results (chart.Online_Set_Size)
             .Running_Total'Image);
      io.Put_Line
        ("pk total:"
         & pk_online_results (chart.Online_Set_Size).Exit_Total'Image
         & " /"
         & pk_online_results (chart.Online_Set_Size).Running_Total'Image);
      io.Put_Line
        ("found offline:"
         & find_max_offline_result.Total_Found'Image
         & " /"
         & find_max_offline_result.Total_Count'Image);

      --  print performance metrics
      io.Put_Line ("time:" & total_time_duration'Image & "s");
      throughput :=
        Float (find_max_offline_result.Total_Count)
        / Float (Ada.Real_Time.To_Duration (total_time_duration));
      io.Put_Line ("throughput:" & throughput'Image);

   end Offline_Solve;

end Solver;
