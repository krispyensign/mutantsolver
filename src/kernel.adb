pragma Ada_2022;

package body Kernel is
   pragma Assertion_Policy (Assert => Check);

   procedure Reset
     (res : in out Kernel_Element'Class; reference_res : Kernel_Element'Class)
   is
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

   procedure Carry_Over_Prices
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class) is
   begin
      res.Entry_Price := last_res.Entry_Price;
      res.Take_Profit_Price := last_res.Take_Profit_Price;
      res.Stop_Loss_Price := last_res.Stop_Loss_Price;
      res.Exit_Total := last_res.Exit_Total;
      pragma Assert (res.Entry_Price /= 0.0);
   end Carry_Over_Prices;

   procedure Carry_Over_Totals
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class) is
   begin
      res.Exit_Total := last_res.Exit_Total;
      res.Min_Exit_Total := last_res.Min_Exit_Total;
      res.Max_Exit_Total := last_res.Max_Exit_Total;
      res.Wins := last_res.Wins;
      res.Losses := last_res.Losses;
      res.Stop_Losses := last_res.Stop_Losses;
      res.Take_Profits := last_res.Take_Profits;
   end Carry_Over_Totals;

   procedure Pin_TPSL_Prices
     (res                    : in out Kernel_Element'Class;
      last_res               : Kernel_Element'Class;
      ask_close              : Long_Float;
      bid_close              : Long_Float;
      atr                    : Long_Float;
      take_profit_multiplier : Float;
      stop_loss_multiplier   : Float;
      num_digits             : Positive;
      behavior               : Common.TPSL_Behavior) is
   begin
      --  pin the entry to the ask close at this tick
      res.Entry_Price := ask_close;

      --  calc the tp and sl prices based on the pinned ask_close price
      if take_profit_multiplier /= 0.0 then
         res.Take_Profit_Price :=
           ask_close + atr * Long_Float (take_profit_multiplier);
      end if;

      if stop_loss_multiplier /= 0.0 then
         res.Stop_Loss_Price :=
           ask_close - atr * Long_Float (stop_loss_multiplier);
      end if;

      if behavior = Common.TPSL_Self_Managed then
         return;
      end if;

      --  if sl is greater than the bid at close the set just below the bid
      --  close price to prevent order rejection
      if res.Stop_Loss_Price > bid_close then
         res.Stop_Loss_Price := bid_close - Long_Float (10.0 ** (-num_digits));
      end if;

      if behavior = Common.TPSL_Dynamic and then last_res.Signal = 1 then
         res.Entry_Price := last_res.Entry_Price;

         --  if the new calculation would exit the position then
         --  revert to original tp
         if take_profit_multiplier /= 0.0
           and then res.Take_Profit_Price < res.Entry_Price
         then
            res.Take_Profit_Price := last_res.Take_Profit_Price;
         end if;

         --  if the new calculation would exit the position then
         --  revert to original sl
         if stop_loss_multiplier /= 0.0
           and then res.Stop_Loss_Price > res.Entry_Price
         then
            res.Stop_Loss_Price := last_res.Stop_Loss_Price;
         end if;
      end if;

      pragma
        Assert
          (take_profit_multiplier = 0.0
             or else res.Take_Profit_Price >= res.Entry_Price);
      pragma
        Assert
          (stop_loss_multiplier = 0.0
             or else res.Entry_Price >= res.Stop_Loss_Price);
   end Pin_TPSL_Prices;

   procedure Trigger_Stop_Loss (res : in out Kernel_Element'Class) is
   begin
      res.Signal := 0;
      res.Trigger := -1;
      res.Exit_Price := res.Stop_Loss_Price;
      res.Stop_Losses := res.Stop_Losses + 1;
      --  pragma Assert (res.Entry_Price >= res.Stop_Loss_Price);
   end Trigger_Stop_Loss;

   procedure Trigger_Take_Profit (res : in out Kernel_Element'Class) is
   begin
      res.Signal := 0;
      res.Trigger := -1;
      res.Exit_Price := res.Take_Profit_Price;
      res.Take_Profits := res.Take_Profits + 1;
      --  pragma Assert (res.Entry_Price <= res.Take_Profit_Price);
   end Trigger_Take_Profit;

   procedure Update_Min_Max_Totals
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class) is
   begin
      if res.Exit_Total /= 0.0
        and then res.Exit_Total > last_res.Max_Exit_Total
      then
         res.Max_Exit_Total := res.Exit_Total;
      end if;

      if res.Exit_Total /= 0.0
        and then res.Exit_Total < last_res.Min_Exit_Total
      then
         res.Min_Exit_Total := res.Exit_Total;
      end if;
   end Update_Min_Max_Totals;

   procedure Update_Wins_Losses (res : in out Kernel_Element'Class) is
   begin
      if res.Exit_Value > 0.0 then
         res.Wins := res.Wins + 1;
      elsif res.Exit_Value < 0.0 then
         res.Losses := res.Losses + 1;
      end if;
   end Update_Wins_Losses;

   procedure Update_Exit_Totals (res : in out Kernel_Element'Class) is
   begin
      res.Exit_Value := res.Exit_Price - res.Entry_Price;
      res.Exit_Total := res.Exit_Total + res.Exit_Value;
      res.Running_Total := res.Exit_Total;
      pragma Assert (res.Exit_Price /= 0.0);
   end Update_Exit_Totals;

   procedure Update_Position
     (res            : in out Kernel_Element'Class;
      bid_exit_price : Long_Float;
      ask_close      : Long_Float) is
   begin
      res.Position := bid_exit_price - ask_close;
      res.Running_Total := res.Exit_Total + res.Position;
      pragma Assert (res.Signal = 1);
   end Update_Position;

   function Calc_WMA_Signal
     (curr           : Common.Keyed_Lane;
      prev           : Common.Keyed_Lane;
      prev_prev      : Common.Keyed_Lane;
      should_roll    : Boolean;
      entry_key      : Common.Candle_Key;
      exit_key       : Common.Candle_Key;
      wma_source_key : Common.WMA_Source_Key;
      last_res       : Kernel_Element) return Kernel_Element
   is
      curr_wma_source  : constant Long_Float :=
        (if should_roll then prev (wma_source_key) else curr (wma_source_key));
      prev_wma_source  : constant Long_Float :=
        (if should_roll then prev_prev (wma_source_key)
         else prev (wma_source_key));
      buy_signal       : constant Boolean :=
        curr (entry_key) > curr_wma_source;
      prior_buy_signal : constant Boolean :=
        prev (entry_key) > prev_wma_source;
      exit_signal      : constant Boolean := curr (exit_key) > curr_wma_source;
      res              : Kernel_Element;
   begin
      res.Signal :=
        (if (not prior_buy_signal and then buy_signal)
           or else (prior_buy_signal and then exit_signal)
         then 1
         else 0);
      res.Trigger := res.Signal - last_res.Signal;
      pragma Assert (res.Trigger'Valid);

      return res;
   end Calc_WMA_Signal;

   procedure Kernel
     (curr      : Common.Keyed_Lane;
      prev      : Common.Keyed_Lane;
      prev_prev : Common.Keyed_Lane;
      conf      : Scenario_Config;
      index     : Positive;
      results   : in out Kernel_Elements)
   is
      bid_exit_price : constant Long_Float :=
        (if conf.Is_Quasi then curr (Common.Bid_Open)
         else curr (Common.Bid_Close));

      res      : Kernel_Element := results (index);
      last_res : Kernel_Element := results (index - 1);

   begin
      --  calculate the wma signal
      res :=
        Calc_WMA_Signal
          (curr,
           prev,
           prev_prev,
           conf.Should_Roll,
           conf.Entry_Key,
           conf.Exit_Key,
           conf.WMA_Source_Key,
           last_res);

      --  prepare the result with the previous result totals
      res.Carry_Over_Totals (last_res);

      if res.Trigger = -1 and then last_res.Trigger = 1 and then conf.Is_Quasi
      then
         --  if quasi and the previous candle is an open and this candle
         --  is a close then erase and bail
         res.Reset (last_res);
         last_res.Reset (last_res);
         results (index) := res;
         results (index - 1) := last_res;

         return;
      end if;

      --  trigger, signal, notes
      --   0, 0, nothing
      --   1, 1, trigger
      --   0, 1, sustain -- check for tp/sl and calc more prices
      --  -1, 0, exit strategy -- check for tp/sl and calc more prices
      if res.Trigger = 0 and then res.Signal = 0 then
         --  nothing is happening currently so bail
         results (index) := res;

         return;
      elsif res.Trigger = 1 and then res.Signal = 1 then
         --  wma cross so pin the prices
         res.Pin_TPSL_Prices
           (last_res               => last_res,
            ask_close              => curr (Common.Ask_Close),
            bid_close              => curr (Common.Bid_Close),
            atr                    => curr (Common.ATR),
            take_profit_multiplier => conf.Take_Profit_Multiplier,
            stop_loss_multiplier   => conf.Stop_Loss_Multiplier,
            num_digits             => conf.Num_Digits,
            behavior               => conf.Exit_Behavior);

         --  pin the entry to the ask close at this tick
         results (index) := res;

         return;
      elsif res.Trigger = -1 and then res.Signal = 0 then
         --  wma cross so set the exit prices
         res.Exit_Price := bid_exit_price;
      end if;

      --  ensure that the state machine is valid
      pragma
        Assert
          (last_res.Signal = 1
             and then ((res.Trigger = -1 and then res.Signal = 0)
                       or else (res.Trigger = 0 and then res.Signal = 1)));

      if conf.Exit_Behavior /= Common.TPSL_Self_Managed then
         --  carry over the prices to continue current open position and or
         --  get ready to exit
         res.Carry_Over_Prices (last_res);

         --  if quasi then the exit already happened and its just being
         --  recorded
         if conf.Is_Quasi then
            if conf.Stop_Loss_Multiplier /= 0.0
              and then (last_res.Stop_Loss_Price > prev (Common.Bid_Low)
                        or else res.Stop_Loss_Price > curr (Common.Bid_Open))
            then
               res.Trigger_Stop_Loss;
            elsif conf.Take_Profit_Multiplier /= 0.0
              and then (last_res.Take_Profit_Price < prev (Common.Bid_High)
                        or else res.Take_Profit_Price < curr (Common.Bid_Open))
            then
               res.Trigger_Take_Profit;
            end if;
         else
            if conf.Stop_Loss_Multiplier /= 0.0
              and then res.Stop_Loss_Price > curr (Common.Bid_Low)
            then
               res.Trigger_Stop_Loss;
            elsif conf.Take_Profit_Multiplier /= 0.0
              and then res.Take_Profit_Price < curr (Common.Bid_High)
            then
               res.Trigger_Take_Profit;
            end if;
         end if;
      else
         --  if self managed then recalc tpsl prior to actually exiting
         res.Pin_TPSL_Prices
           (last_res               => last_res,
            ask_close              => curr (Common.Ask_Close),
            bid_close              => curr (Common.Bid_Close),
            atr                    => curr (Common.ATR),
            take_profit_multiplier => conf.Take_Profit_Multiplier,
            stop_loss_multiplier   => conf.Stop_Loss_Multiplier,
            num_digits             => conf.Num_Digits,
            behavior               => conf.Exit_Behavior);
         res.Entry_Price := last_res.Entry_Price;

         --  if quasi then the exit already happened and its just being
         --  recorded
         if conf.Is_Quasi then
            if conf.Stop_Loss_Multiplier /= 0.0
              and then res.Stop_Loss_Price > curr (Common.Bid_Open)
            then
               res.Signal := 0;
               res.Trigger := -1;
               res.Exit_Price := curr (Common.Bid_Open);
               res.Stop_Losses := res.Stop_Losses + 1;
            elsif conf.Take_Profit_Multiplier /= 0.0
              and then res.Take_Profit_Price < curr (Common.Bid_Open)
            then
               res.Signal := 0;
               res.Trigger := -1;
               res.Exit_Price := curr (Common.Bid_Open);
               res.Take_Profits := res.Take_Profits + 1;
            end if;
         else
            if conf.Stop_Loss_Multiplier /= 0.0
              and then res.Stop_Loss_Price > curr (Common.Bid_Close)
            then
               res.Signal := 0;
               res.Trigger := -1;
               res.Exit_Price := curr (Common.Bid_Close);
               res.Stop_Losses := res.Stop_Losses + 1;
            elsif conf.Take_Profit_Multiplier /= 0.0
              and then res.Take_Profit_Price < curr (Common.Bid_Close)
            then
               res.Signal := 0;
               res.Trigger := -1;
               res.Exit_Price := curr (Common.Bid_Close);
               res.Take_Profits := res.Take_Profits + 1;
            end if;
         end if;
      end if;

      --  update the exit totals and or positions
      if res.Trigger = -1 then
         res.Update_Exit_Totals;
      else
         if conf.Exit_Behavior = Common.TPSL_Dynamic then
            --  if dynamic then re-pin the tpsl prices
            res.Pin_TPSL_Prices
              (last_res               => last_res,
               ask_close              => curr (Common.Ask_Close),
               bid_close              => curr (Common.Bid_Close),
               atr                    => curr (Common.ATR),
               take_profit_multiplier => conf.Take_Profit_Multiplier,
               stop_loss_multiplier   => conf.Stop_Loss_Multiplier,
               num_digits             => conf.Num_Digits,
               behavior               => conf.Exit_Behavior);
         end if;

         res.Update_Position (bid_exit_price, curr (Common.Ask_Close));
      end if;

      --  update totals and wins/losses
      res.Update_Min_Max_Totals (last_res);
      res.Update_Wins_Losses;

      results (index) := res;

   end Kernel;

end Kernel;
