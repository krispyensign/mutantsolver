pragma Ada_2022;

with Ada.Text_IO;

package body Kernel_Ops is
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
      sr           : Scenario_Report;
   begin
      --  prevent sl > tp
      --  TODO make this toggle
      --  if stop_loss_multiplier > take_profit_multiplier then
      --     return;
      --  end if;

      --  log progress
      result.total_count := result.total_count + 1;
      if result.total_count mod 100000 = 0 then
         io.Put_Line (result.total_count'Image);
      end if;

      --  determine quasi and if we should roll
      is_quasi := Is_In_Quasi_Keys (exit_key);
      if is_quasi then
         should_roll := Is_Not_In_WMA_Quasi_Keys (wma_source_key);
      end if;

      --  prepare the scenario config
      conf :=
        (Start_Index            => chart.Time_Period_Interval,
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
      sr :=
        (Wins           => last_element.Wins,
         Losses         => last_element.Losses,
         Max_Exit_Total => last_element.Max_Exit_Total,
         Min_Exit_Total => last_element.Min_Exit_Total,
         Final_Total    => last_element.Exit_Total,
         Take_Profits   => last_element.Take_Profits,
         Stop_Losses    => last_element.Stop_Losses,
         Ratio          => 0.0,
         Config         => conf);
      result.last_scenario_report := sr;
      result.total_reported := result.total_reported + 1;

      --  if the final total is first then this is the first report
      if result.best_scenario_report.Final_Total = Long_Float'First then
         result.best_scenario_report := sr;
      end if;

      --  if the final total is greater than zero then increment the found
      if sr.Final_Total > 0.0 then
         result.total_found := result.total_found + 1;
      end if;

      --  if the best scenario report is less than or equal to zero
      --  and the current scenario report is greater than the best
      --  then update the best scenario report
      if result.best_scenario_report.Final_Total <= 0.0
        and then sr.Final_Total > result.best_scenario_report.Final_Total
      then
         result.best_scenario_report := sr;
         io.Put_Line (result.best_scenario_report'Image);
      end if;

      --  if the final total is less than or equal to zero then bail
      --  or if the max exit total is less than or equal to the absolute value
      --  of the min exit total then bail
      if sr.Final_Total <= 0.0
        or else sr.Max_Exit_Total <= abs (sr.Min_Exit_Total)
      then
         return;
      end if;

      --  calculate the ratio
      ratio :=
        (if sr.Wins + sr.Losses > 0
         then Float (sr.Wins) / Float (sr.Wins + sr.Losses)
         else 0.0);
      
      --  if the ratio is greater than the best scenario report ratio
      --  and the final total is greater than the best scenario report final total
      --  then update the best scenario report
      if ratio > result.best_scenario_report.Ratio
        and then sr.Final_Total > result.best_scenario_report.Final_Total
      then
         result.best_scenario_report := sr;
         result.best_scenario_report.Ratio := ratio;
         io.Put_Line (result.best_scenario_report'Image);
      end if;

   end Process_Kernel_Operation;

   function Find_Max
     (p : Common.Row_Pool; chart : Config.Chart_Config) return Operation_Result
   is
      temp_total_found : Natural := 0;
      result           : Operation_Result;

   begin
      --  queue the configs to the solver tasks
      result.total_found := 0;
      for wma_source_key in Common.WMA_Source_Key'Range loop
         for entry_key in Common.Candle_Key'Range loop
            for exit_key in Common.Candle_Key'Range loop
               for take_profit_multiplier of Common.Take_Profit_Multipliers
               loop
                  for stop_loss_multiplier of Common.Stop_Loss_Multipliers loop
                     Process_Kernel_Operation
                       (p,
                        result,
                        chart,
                        entry_key,
                        exit_key,
                        wma_source_key,
                        take_profit_multiplier,
                        stop_loss_multiplier);
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

      return result;

   end Find_Max;
end Kernel_Ops;
