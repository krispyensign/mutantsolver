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
   online_data_pool : online_p.Pool;

   best_scenario        : Core.Scenario;
   best_scenario_result : Core.Scenario_Result (1 .. count);
   best_scenario_report : Core.Scenario_Report := (
      Wins => 0,
      Losses => 0,
      Final_Total => 0.0,
      Ratio => 0.0,
      Max_Exit_Total => 0.0,
      Min_Exit_Total => 0.0);

   one_true_count      : Natural := 0;
   total_found         : Natural := 0;
   start_time          : Ada.Real_Time.Time;
   end_time            : Ada.Real_Time.Time;
   total_time_duration : Ada.Real_Time.Time_Span;

begin
   --  fetch the candles
   ex_candles := Oanda_Exchange.Fetch_Candles (oanda, chart);

   --  populate the full data pool from the retrieved candles
   full_data_pool := full_p.Make_Pool (ex_candles, chart.Time_Period_Interval);

   --  partition the offline data pool
   offline_data_pool :=
     [for i in Common.Pool_Key'Range =>
        offline_p.Swim_Lane
          (full_data_pool (i) (1 .. chart.Offline_Set_Size))];
   --  populate the tp/sl offline data pool with a segment of the main
   --  offline pool
   tp_sl_offline_data_pool :=
     [for i in Common.Pool_Key'Range =>
        tp_sl_offline_p.Swim_Lane
          (offline_data_pool (i)
             (chart.Offline_Set_Size
              - chart.TP_SL_Offline_Set_Size
              + 1
              .. chart.Offline_Set_Size))];

   --  poplate the simulated online data pool for zero knowledge tests
   online_data_pool :=
     [for i in Common.Pool_Key'Range =>
        online_p.Swim_Lane
          (full_data_pool (i) (chart.Offline_Set_Size + 1 .. count))];

   start_time := Ada.Real_Time.Clock;
   for entry_key in Common.Candle_Key'Range loop
      for exit_key in Common.Candle_Key'Range loop
         for wma_source_key in Common.WMA_Source_Key'Range loop
            declare
               --  current scenario inputs
               cur_scen : constant Core.Scenario :=
                 (Is_Quasi               => False,
                  Take_Profit_Multiplier => 0.0,
                  Stop_Loss_Multiplier   => 0.0,
                  Entry_Key              => entry_key,
                  Exit_Key               => exit_key,
                  WMA_Source_Key         => wma_source_key,
                  Num_Digits             => chart.Num_Digits);

               --  current scenario result
               cur_scen_res :
                 Core.Scenario_Result (1 .. chart.Offline_Set_Size);
               last_element : Core.Scenario_Result_Element;

               max_exit_total : Long_Float := Long_Float'First;
               min_exit_total : Long_Float := Long_Float'Last;
               wins           : Natural := 0;
               losses         : Natural := 0;
               ratio          : Float := 0.0;
               final_total    : Long_Float := 0.0;

            begin
               one_true_count := one_true_count + 1;

               for i in chart.Time_Period_Interval .. chart.Offline_Set_Size
               loop
                  --  calculate iteration
                  offline_p.Kernel
                    (offline_data_pool, i, cur_scen, cur_scen_res);

                  --  update max total
                  if cur_scen_res (i).Exit_Total > max_exit_total then
                     max_exit_total := cur_scen_res (i).Exit_Total;
                  end if;

                  --  update min total
                  if cur_scen_res (i).Exit_Total < min_exit_total then
                     min_exit_total := cur_scen_res (i).Exit_Total;
                  end if;

                  --  update wins and losses
                  if cur_scen_res (i).Exit_Value > 0.0 then
                     wins := wins + 1;
                  elsif cur_scen_res (i).Exit_Value < 0.0 then
                     losses := losses + 1;
                  end if;

                  last_element := cur_scen_res (i);

               end loop;

               --  skip further processing if criteria is not met
               last_element := cur_scen_res (chart.Offline_Set_Size);
               final_total := last_element.Exit_Total;
               if final_total <= 0.0
                 or else abs (max_exit_total) <= abs (min_exit_total)
               then
                  goto Continue;
               end if;

               total_found := total_found + 1;
               ratio :=
                 (if (wins + losses) > 0
                  then Float (wins / (wins + losses))
                  else Float (0.0));

               if (best_scenario_report.Final_Total = 0.0)
                 or else (ratio >= best_scenario_report.Ratio
                 and then final_total >= best_scenario_report.Final_Total)
               then
                  best_scenario_report :=
                    (Wins           => wins,
                     Losses         => losses,
                     Max_Exit_Total => max_exit_total,
                     Min_Exit_Total => min_exit_total,
                     Ratio          => ratio,
                     Final_Total    => final_total);
               end if;

               io.Put_Line ("----");
               io.Put_Line (final_total'Image);
               io.Put_Line (ratio'Image);

               <<Continue>>
            end;

            --  check best versus current
         end loop;
      end loop;
   end loop;
   end_time := Ada.Real_Time.Clock;
   total_time_duration := Ada.Real_Time."-" (end_time, start_time);

   io.Put_Line (one_true_count'Image);
   io.Put_Line (total_found'Image);
   io.Put_Line (total_time_duration'Image);

end Mutantsolver;
