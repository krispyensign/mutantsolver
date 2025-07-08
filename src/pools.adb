pragma Ada_2022;
with TA;

package body Pools is

   procedure Pin_Prices
     (p : Pool; i : Positive; s : Core.Scenario; res : in out Core.Scenario_Result) is
   begin
      -- pin the entry to the ask close at this tick
      res.Entry_Value := p (Common.Ask_Close) (i);

      -- calc the tp and sl prices based on the pinned ask_close price
      res.Take_Profit_Price :=
        p (Common.Ask_Close) (i)
        + p (Common.ATR) (i) * Long_Float (s.Take_Profit_Multiplier);
      res.Stop_Loss_Price :=
        p (Common.Ask_Close) (i)
        - p (Common.ATR) (i) * Long_Float (s.Stop_Loss_Multiplier);

      -- if sl is greater than the bid at close the set just below the bid
      -- close price to prevent order rejection
      if res.Stop_Loss_Price > p (Common.Bid_Close) (i) then
         res.Stop_Loss_Price :=
           p (Common.Bid_Close) (i) - Long_Float (10.0 ** (-s.num_digits));
      end if;

   end Pin_Prices;

   function Calc_WMA_Signal
     (p        : Pool;
      i        : Positive;
      s        : Core.Scenario;
      last_res : Core.Scenario_Result) return Core.Scenario_Result
   is
      buy_signal       : constant Boolean :=
        p (s.Entry_Key) (i) > p (s.WMA_Source_Key) (i);
      prior_buy_signal : constant Boolean :=
        p (s.Entry_Key) (i - 1) > p (s.WMA_Source_Key) (i - 1);
      exit_signal      : constant Boolean :=
        p (s.Exit_Key) (i) > p (s.WMA_Source_Key) (i);
      res : Core.Scenario_Result;
   begin
      res.Signal :=
        (if (not prior_buy_signal and buy_signal)
           or (prior_buy_signal and exit_signal)
         then 1
         else 0);
      res.Trigger := res.Signal - last_res.Signal;

      return res;
   end Calc_WMA_Signal;

   procedure Kernel
     (p        : Pool;
      i        : Positive;
      s        : Core.Scenario;
      res      : out Core.Scenario_Result;
      last_res : in out Core.Scenario_Result) is
   begin
      -- calculate the wma signal
      res := Calc_WMA_Signal (p, i, s, last_res);

      -- if quasi and the previous candle is an open and this candle
      -- is a close then erase and bail
      if res.Trigger = -1 and last_res.Trigger = 1 and s.Is_Quasi then
         res.Reset;
         last_res.Reset;
         return;

      -- nothing is happening currently so exit
      elsif res.Trigger = 0 and res.Signal = 0 then
         return;

      -- wma cross so trigger and pin the prices
      elsif res.Trigger = 1 and res.Signal = 1 then
         Pin_Prices(p, i, s, res);

         return;

      elsif res.Trigger = 0 and res.Signal = 1 then
         res.Set_Prices (last_res);

         return;
      end if;

      -- TODO finish this part

   end Kernel;

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
