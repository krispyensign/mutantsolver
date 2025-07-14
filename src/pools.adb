pragma Ada_2022;
with TA;
with Ada.Text_IO;
with Reporting;

package body Pools is
   package io renames Ada.Text_IO;

   task body Process_Kernel is
      procedure Start_Internal (p : Pool; conf : Core.Scenario_Config) is
         cur_scen_res    : Core.Scenario_Result (1 .. Count) :=
           [others => <>];
         last_element    : Core.Scenario_Result_Element;
         final_total     : Long_Float := 0.0;
         is_found        : Boolean := False;
         scenario_report : Core.Scenario_Report;
      begin
         for i in conf.Start_Index .. Count loop
            --  calculate iteration
            Core.Kernel
              (curr    =>
                 (WMA_Component_Price   => p (conf.WMA_Source_Key) (i),
                  Entry_Component_Price => p (conf.Entry_Key) (i),
                  Exit_Component_Price  => p (conf.Exit_Key) (i),
                  ATR                   => p (Common.ATR) (i),
                  Ask_Close             => p (Common.Ask_Close) (i),
                  Bid_Open              => p (Common.Bid_Close) (i),
                  Bid_High              => p (Common.Bid_High) (i),
                  Bid_Low               => p (Common.Bid_Low) (i),
                  Bid_Close             => p (Common.Bid_Close) (i)),
               prev    =>
                 (WMA_Component_Price   => p (conf.WMA_Source_Key) (i - 1),
                  Entry_Component_Price => p (conf.Entry_Key) (i - 1),
                  Exit_Component_Price  => p (conf.Exit_Key) (i - 1),
                  ATR                   => p (Common.ATR) (i - 1),
                  Ask_Close             => p (Common.Ask_Close) (i - 1),
                  Bid_Open              => p (Common.Bid_Close) (i - 1),
                  Bid_High              => p (Common.Bid_High) (i - 1),
                  Bid_Low               => p (Common.Bid_Low) (i - 1),
                  Bid_Close             => p (Common.Bid_Close) (i - 1)),
               index   => i,
               conf    => conf,
               results => cur_scen_res);
         end loop;

         --  skip further processing if criteria is not met
         last_element := cur_scen_res (Count);
         scenario_report :=
           (Wins           => last_element.Wins,
            Losses         => last_element.Losses,
            Max_Exit_Total => last_element.Max_Exit_Total,
            Min_Exit_Total => last_element.Min_Exit_Total,
            Ratio          => last_element.Ratio,
            Final_Total    => last_element.Exit_Total,
            Config         => conf);
         Reporting.Reporting.Update_Scenario (scenario_report);
      end Start_Internal;
   begin
      loop
         select
            accept Start (p : Pool; conf : Core.Scenario_Config) do
               Start_Internal (p, conf);
            end Start;
         or
               terminate;
         end select;
      end loop;
   end Process_Kernel;

   function Make_Pool
     (ex_candles : Core.Candles; time_interval_period : Positive) return Pool
   is
      --  apply the heiken ashi transform
      int_ha_candles : constant Core.HA_Candles (1 .. Count) :=
        Core.Make_HA_Candles (ex_candles);
      --  convert candle records to pool object (record format to
      --  column format)
      full_data_pool : Pool :=
        [Common.Ask_Open     =>
           [for i in 1 .. Count => ex_candles (i).Ask_Open],
         Common.Ask_High     =>
           [for i in 1 .. Count => ex_candles (i).Ask_High],
         Common.Ask_Low      =>
           [for i in 1 .. Count => ex_candles (i).Ask_Low],
         Common.Ask_Close    =>
           [for i in 1 .. Count => ex_candles (i).Ask_Close],
         Common.Mid_Open     =>
           [for i in 1 .. Count => ex_candles (i).Mid_Open],
         Common.Mid_High     =>
           [for i in 1 .. Count => ex_candles (i).Mid_High],
         Common.Mid_Low      =>
           [for i in 1 .. Count => ex_candles (i).Mid_Low],
         Common.Mid_Close    =>
           [for i in 1 .. Count => ex_candles (i).Mid_Close],
         Common.Bid_Open     =>
           [for i in 1 .. Count => ex_candles (i).Bid_Open],
         Common.Bid_High     =>
           [for i in 1 .. Count => ex_candles (i).Bid_High],
         Common.Bid_Low      =>
           [for i in 1 .. Count => ex_candles (i).Bid_Low],
         Common.Bid_Close    =>
           [for i in 1 .. Count => ex_candles (i).Bid_Close],
         Common.HA_Ask_Open  =>
           [for i in 1 .. Count => int_ha_candles (i).Ask_Open],
         Common.HA_Ask_High  =>
           [for i in 1 .. Count => int_ha_candles (i).Ask_High],
         Common.HA_Ask_Low   =>
           [for i in 1 .. Count => int_ha_candles (i).Ask_Low],
         Common.HA_Ask_Close =>
           [for i in 1 .. Count => int_ha_candles (i).Ask_Close],
         Common.HA_Mid_Open  =>
           [for i in 1 .. Count => int_ha_candles (i).Mid_Open],
         Common.HA_Mid_High  =>
           [for i in 1 .. Count => int_ha_candles (i).Mid_High],
         Common.HA_Mid_Low   =>
           [for i in 1 .. Count => int_ha_candles (i).Mid_Low],
         Common.HA_Mid_Close =>
           [for i in 1 .. Count => int_ha_candles (i).Mid_Close],
         Common.HA_Bid_Open  =>
           [for i in 1 .. Count => int_ha_candles (i).Bid_Open],
         Common.HA_Bid_High  =>
           [for i in 1 .. Count => int_ha_candles (i).Bid_High],
         Common.HA_Bid_Low   =>
           [for i in 1 .. Count => int_ha_candles (i).Bid_Low],
         Common.HA_Bid_Close =>
           [for i in 1 .. Count => int_ha_candles (i).Bid_Close],
         others              => [for i in 1 .. Count => 0.0]];

   begin
      --  Calculate the ATR
      full_data_pool (Common.ATR) :=
        Swim_Lane
          (TA.Calc_TA_ATR
             (in_high     =>
                Common.Real_Array (full_data_pool (Common.Mid_High)),
              in_low      =>
                Common.Real_Array (full_data_pool (Common.Mid_Low)),
              in_close    =>
                Common.Real_Array (full_data_pool (Common.Mid_Close)),
              time_period => time_interval_period));

      full_data_pool (Common.WMA_Ask_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Ask_Open)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_Ask_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Ask_High)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_Ask_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Ask_Low)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_Ask_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Ask_Close)),
              time_period => time_interval_period));

      full_data_pool (Common.WMA_Mid_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Mid_Open)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_Mid_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Mid_High)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_Mid_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Mid_Low)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_Mid_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Mid_Close)),
              time_period => time_interval_period));

      full_data_pool (Common.WMA_Bid_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Bid_Open)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_Bid_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Bid_High)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_Bid_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Bid_Low)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_Bid_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.Bid_Close)),
              time_period => time_interval_period));

      full_data_pool (Common.WMA_HA_Ask_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Ask_Open)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_HA_Ask_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Ask_High)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_HA_Ask_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Ask_Low)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_HA_Ask_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Ask_Close)),
              time_period => time_interval_period));

      full_data_pool (Common.WMA_HA_Mid_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Mid_Open)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_HA_Mid_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Mid_High)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_HA_Mid_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Mid_Low)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_HA_Mid_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Mid_Close)),
              time_period => time_interval_period));

      full_data_pool (Common.WMA_HA_Bid_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Bid_Open)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_HA_Bid_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Bid_High)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_HA_Bid_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Bid_Low)),
              time_period => time_interval_period));
      full_data_pool (Common.WMA_HA_Bid_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Common.Real_Array (full_data_pool (Common.HA_Bid_Close)),
              time_period => time_interval_period));

      return full_data_pool;
   end Make_Pool;
end Pools;
