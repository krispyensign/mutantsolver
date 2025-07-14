pragma Ada_2022;

package body Core is

   procedure Reset
     (res           : in out Scenario_Result_Element'Class;
      reference_res : Scenario_Result_Element'Class) is
   begin
      res.Entry_Price := 0.0;
      res.Exit_Price := 0.0;
      res.Trigger := 0;
      res.Signal := 0;
      res.Take_Profit_Price := 0.0;
      res.Stop_Loss_Price := 0.0;
      res.Position := 0.0;
      res.Exit_Value := 0.0;
      res.Exit_Total := reference_res.Exit_Total;
      res.Running_Total := reference_res.Exit_Total;
   end Reset;

   procedure Set_Prices
     (res      : in out Scenario_Result_Element'Class;
      last_res : Scenario_Result_Element'Class) is
   begin
      res.Entry_Price := last_res.Entry_Price;
      res.Take_Profit_Price := last_res.Take_Profit_Price;
      res.Stop_Loss_Price := last_res.Stop_Loss_Price;
      res.Exit_Total := last_res.Exit_Total;
   end Set_Prices;
   procedure Pin_Prices
     (curr                   : Core.Kernel_Element;
      take_profit_multiplier : Float;
      stop_loss_multiplier   : Float;
      num_digits             : Positive;
      res                    : in out Core.Scenario_Result_Element) is
   begin
      --  pin the entry to the ask close at this tick
      res.Entry_Price := curr.Ask_Close;

      --  calc the tp and sl prices based on the pinned ask_close price
      if take_profit_multiplier /= 0.0 then
         res.Take_Profit_Price :=
           curr.Ask_Close + curr.ATR * Long_Float (take_profit_multiplier);
      end if;

      if stop_loss_multiplier /= 0.0 then
         res.Stop_Loss_Price :=
           curr.Ask_Close - curr.ATR * Long_Float (stop_loss_multiplier);
      end if;

      --  if sl is greater than the bid at close the set just below the bid
      --  close price to prevent order rejection
      if res.Stop_Loss_Price > curr.Bid_Close then
         res.Stop_Loss_Price :=
           curr.Bid_Close - Long_Float (10.0 ** (-num_digits));
      end if;

   end Pin_Prices;

   function Calc_WMA_Signal
     (curr           : Core.Kernel_Element;
      prev           : Core.Kernel_Element;
      entry_key      : Common.Candle_Key;
      exit_key       : Common.Candle_Key;
      wma_source_key : Common.WMA_Source_Key;
      last_res       : Core.Scenario_Result_Element)
      return Core.Scenario_Result_Element
   is
      buy_signal       : constant Boolean :=
        curr.Entry_Component_Price > curr.WMA_Component_Price;
      prior_buy_signal : constant Boolean :=
        prev.Entry_Component_Price > prev.WMA_Component_Price;
      exit_signal      : constant Boolean :=
        curr.Exit_Component_Price > curr.WMA_Component_Price;
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
     (curr    : Core.Kernel_Element;
      prev    : Core.Kernel_Element;
      conf    : Core.Scenario_Config;
      index   : Positive;
      results : in out Core.Scenario_Result)
   is
      bid_exit_price : constant Long_Float :=
        (if conf.is_quasi then curr.Bid_Open else curr.Bid_Close);
      res            : Core.Scenario_Result_Element := results (index);
      last_res       : Core.Scenario_Result_Element := results (index - 1);

   begin
      --  calculate the wma signal
      res :=
        Calc_WMA_Signal
          (curr,
           prev,
           conf.entry_key,
           conf.exit_key,
           conf.wma_source_key,
           last_res);
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
      if res.Trigger = -1 and then last_res.Trigger = 1 and then conf.is_quasi
      then
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
           (curr,
            conf.take_profit_multiplier,
            conf.stop_loss_multiplier,
            conf.num_digits,
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
      if conf.stop_loss_multiplier /= 0.0
        and then curr.Bid_Low < res.Stop_Loss_Price
      then
         res.Signal := 0;
         res.Trigger := -1;
         res.Exit_Price := res.Stop_Loss_Price;

      elsif conf.take_profit_multiplier /= 0.0
        and then curr.Bid_High > res.Take_Profit_Price
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
         res.Position := bid_exit_price - curr.Ask_Close;
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

   function Make_HA_Candles (in_candles : Candles) return HA_Candles is
      temp_ha_candles : HA_Candles :=
        [for i in 1 .. in_candles'Length
         => (if i = 1 then Make_HA_Candle (in_candles (1), in_candles (1))
             else Make_HA_Candle (in_candles (i), in_candles (i - 1)))];
   begin
      return temp_ha_candles;
   end Make_HA_Candles;

   function Make_HA_Candle
     (current_candle : Candle_Base'Class; previous_candle : Candle_Base'Class)
      return HA_Candle is
   begin
      return
        (Volume    => current_candle.Volume,
         Complete  => current_candle.Complete,
         Time      => current_candle.Time,
         Ask_Open  =>
           (previous_candle.Ask_Open + previous_candle.Ask_Close) / 2.0,
         Ask_High  =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Ask_Open, current_candle.Ask_Close),
              current_candle.Ask_High),
         Ask_Low   =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Ask_Open, current_candle.Ask_Close),
              current_candle.Ask_Low),
         Ask_Close =>
           (current_candle.Ask_Open + current_candle.Ask_High
            + current_candle.Ask_Low
            + current_candle.Ask_Close)
           / 4.0,
         Mid_Open  =>
           (previous_candle.Mid_Open + previous_candle.Mid_Close) / 2.0,
         Mid_High  =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Mid_Open, current_candle.Mid_Close),
              current_candle.Mid_High),
         Mid_Low   =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Mid_Open, current_candle.Mid_Close),
              current_candle.Mid_Low),
         Mid_Close =>
           (current_candle.Mid_Open + current_candle.Mid_High
            + current_candle.Mid_Low
            + current_candle.Mid_Close)
           / 4.0,
         Bid_Open  =>
           (previous_candle.Bid_Open + previous_candle.Bid_Close) / 2.0,
         Bid_High  =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Bid_Open, current_candle.Bid_Close),
              current_candle.Bid_High),
         Bid_Low   =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Bid_Open, current_candle.Bid_Close),
              current_candle.Bid_Low),
         Bid_Close =>
           (current_candle.Bid_Open + current_candle.Bid_High
            + current_candle.Bid_Low
            + current_candle.Bid_Close)
           / 4.0);
   end Make_HA_Candle;

end Core;
