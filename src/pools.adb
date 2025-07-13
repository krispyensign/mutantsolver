pragma Ada_2022;
with TA;

package body Pools is

   procedure Pin_Prices
     (p                      : Pool;
      i                      : Positive;
      take_profit_multiplier : Float;
      stop_loss_multiplier   : Float;
      num_digits             : Positive;
      res                    : in out Core.Scenario_Result_Element) is
   begin
      --  pin the entry to the ask close at this tick
      res.Entry_Price := p (Common.Ask_Close) (i);

      --  calc the tp and sl prices based on the pinned ask_close price
      if take_profit_multiplier /= 0.0 then
         res.Take_Profit_Price :=
           p (Common.Ask_Close) (i)
           + p (Common.ATR) (i) * Long_Float (take_profit_multiplier);
      end if;

      if stop_loss_multiplier /= 0.0 then
         res.Stop_Loss_Price :=
           p (Common.Ask_Close) (i)
           - p (Common.ATR) (i) * Long_Float (stop_loss_multiplier);
      end if;

      --  if sl is greater than the bid at close the set just below the bid
      --  close price to prevent order rejection
      if res.Stop_Loss_Price > p (Common.Bid_Close) (i) then
         res.Stop_Loss_Price :=
           p (Common.Bid_Close) (i) - Long_Float (10.0 ** (-num_digits));
      end if;

   end Pin_Prices;

   function Calc_WMA_Signal
     (p              : Pool;
      i              : Positive;
      entry_key      : Common.Candle_Key;
      exit_key       : Common.Candle_Key;
      wma_source_key : Common.WMA_Source_Key;
      last_res       : Core.Scenario_Result_Element)
      return Core.Scenario_Result_Element
   is
      buy_signal       : constant Boolean :=
        p (entry_key) (i) > p (wma_source_key) (i);
      prior_buy_signal : constant Boolean :=
        p (entry_key) (i - 1) > p (wma_source_key) (i - 1);
      exit_signal      : constant Boolean :=
        p (exit_key) (i) > p (wma_source_key) (i);
      res              : Core.Scenario_Result_Element;
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
     (p                      : Pool;
      index                  : Positive;
      entry_key              : Common.Candle_Key;
      exit_key               : Common.Candle_Key;
      wma_source_key         : Common.WMA_Source_Key;
      take_profit_multiplier : Float;
      stop_loss_multiplier   : Float;
      num_digits             : Positive;
      is_quasi               : Boolean;
      results                : in out Core.Scenario_Result)
   is
      bid_low_price  : constant Long_Float := p (Common.Bid_Low) (index);
      bid_high_price : constant Long_Float := p (Common.Bid_High) (index);
      bid_exit_price : constant Long_Float :=
        p (if is_quasi then Common.Bid_Open else Common.Bid_Close) (index);
      res            : Core.Scenario_Result_Element := results (index);
      last_res       : Core.Scenario_Result_Element := results (index - 1);

   begin
      --  calculate the wma signal
      res :=
        Calc_WMA_Signal
          (p, index, entry_key, exit_key, wma_source_key, last_res);
      res.Exit_Total := last_res.Exit_Total;
      res.Min_Exit_Total := last_res.Min_Exit_Total;
      res.Max_Exit_Total := last_res.Max_Exit_Total;
      res.Wins := last_res.Wins;
      res.Losses := last_res.Losses;
      res.Ratio := last_res.Ratio;

      --  trigger, signal, notes
      --   0, 0, nothing
      --   1, 1, trigger
      --   0, 1, sustain -- check for tp/sl and calc more prices
      --  -1, 0, exit strategy -- check for tp/sl and calc more prices
      if res.Trigger = -1 and then last_res.Trigger = 1 and then is_quasi then
         --  if quasi and the previous candle is an open and this candle
         --  is a close then erase and bail
         res.Reset (last_res);
         last_res.Reset (last_res);
         results (index) := res;
         results (index - 1) := last_res;

         return;
      elsif res.Trigger = 0 and then res.Signal = 0 then
         --  nothing is happening currently so bail
         results (index) := res;

         return;
      elsif res.Trigger = 1 and then res.Signal = 1 then
         --  wma cross so pin the prices
         Pin_Prices
           (p,
            index,
            take_profit_multiplier,
            stop_loss_multiplier,
            num_digits,
            res);
         results (index) := res;

         return;
      elsif res.Trigger = -1 and then res.Signal = 0 then
         --  wma cross so set the exit prices
         res.Exit_Price := bid_exit_price;
      end if;

      --  set the prices
      res.Set_Prices (last_res);

      --  check for tp/sl
      if stop_loss_multiplier /= 0.0
        and then bid_low_price < res.Stop_Loss_Price
      then
         res.Signal := 0;
         res.Trigger := -1;
         res.Exit_Price := res.Stop_Loss_Price;

      elsif take_profit_multiplier /= 0.0
        and then bid_high_price > res.Take_Profit_Price
      then
         res.Signal := 0;
         res.Trigger := -1;
         res.Exit_Price := res.Take_Profit_Price;
      end if;

      --  update the running and exit totals
      if res.Exit_Price /= 0.0 then
         res.Exit_Value := res.Exit_Price - res.Entry_Price;
         res.Exit_Total := res.Exit_Total + res.Exit_Value;
         res.Running_Total := res.Exit_Total;
      else
         res.Position := bid_exit_price - p (Common.Ask_Close) (index);
         res.Running_Total := res.Exit_Total + res.Position;
      end if;

      --  update max total
      if res.Exit_Total > last_res.Max_Exit_Total then
         res.Max_Exit_Total := res.Exit_Total;
      end if;

      --  update min total
      if res.Exit_Total < last_res.Min_Exit_Total then
         res.Min_Exit_Total := res.Exit_Total;
      end if;

      --  update wins and losses
      if res.Exit_Value > 0.0 then
         res.Wins := res.Wins + 1;
      elsif res.Exit_Value < 0.0 then
         res.Losses := res.Losses + 1;
      end if;

      res.Ratio :=
        (if (res.Wins + res.Losses) > 0
         then Float (res.Wins / (res.Wins + res.Losses))
         else Float (0.0));

      results (index) := res;

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
