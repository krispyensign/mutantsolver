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
      res.Running_Total := reference_res.Running_Total;
      res.Crosses := reference_res.Crosses;
      res.Entries := reference_res.Entries;
      res.Wins := reference_res.Wins;
      res.Losses := reference_res.Losses;
      res.Stop_Losses := reference_res.Stop_Losses;
      res.Take_Profits := reference_res.Take_Profits;
      res.Min_Exit_Total := reference_res.Min_Exit_Total;
      res.Max_Exit_Total := reference_res.Max_Exit_Total;
   end Reset;

   procedure Carry_Over_Prices
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class) is
   begin
      res.Entry_Price := last_res.Entry_Price;
      res.Take_Profit_Price := last_res.Take_Profit_Price;
      res.Stop_Loss_Price := last_res.Stop_Loss_Price;
      pragma Assert (res.Entry_Price /= 0.0);
   end Carry_Over_Prices;

   procedure Carry_Over_Totals
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class) is
   begin
      res.Exit_Total := last_res.Exit_Total;
      res.Running_Total := last_res.Running_Total;
      res.Min_Exit_Total := last_res.Min_Exit_Total;
      res.Max_Exit_Total := last_res.Max_Exit_Total;
      res.Wins := last_res.Wins;
      res.Losses := last_res.Losses;
      res.Stop_Losses := last_res.Stop_Losses;
      res.Take_Profits := last_res.Take_Profits;
      res.Entries := last_res.Entries;
      res.Crosses := last_res.Crosses;
   end Carry_Over_Totals;

   procedure Calc_TPSL_Prices
     (res      : in out Kernel_Element'Class;
      last_res : Kernel_Element'Class;
      curr     : Common.Keyed_Lane;
      conf     : Scenario_Config) is
   begin
      --  calc the tp and sl prices based on the pinned ask_close price
      if conf.Take_Profit_Multiplier /= 0.0 then
         res.Take_Profit_Price :=
           curr (Common.Ask_Close)
           + curr (Common.ATR) * Long_Float (conf.Take_Profit_Multiplier);
      end if;

      if conf.Stop_Loss_Multiplier /= 0.0 then
         res.Stop_Loss_Price :=
           curr (Common.Ask_Close)
           - curr (Common.ATR) * Long_Float (conf.Stop_Loss_Multiplier);
      end if;

      if conf.Exit_Behavior = Common.TPSL_Self_Managed then
         return;
      end if;

      --  if sl is greater than the bid at close the set just below the bid
      --  close price to prevent order rejection
      if res.Stop_Loss_Price > curr (Common.Bid_Close) then
         res.Stop_Loss_Price :=
           curr (Common.Bid_Close) - Long_Float (10.0 ** (-conf.Num_Digits));
      end if;

      if conf.Exit_Behavior = Common.TPSL_Dynamic and then last_res.Signal = 1
      then
         --  if the new calculation would exit the position then
         --  revert to original tp
         if conf.Take_Profit_Multiplier /= 0.0
           and then res.Take_Profit_Price < res.Entry_Price
         then
            res.Take_Profit_Price := last_res.Take_Profit_Price;
         end if;

         --  if the new calculation would exit the position then
         --  revert to original sl
         if conf.Stop_Loss_Multiplier /= 0.0
           and then res.Stop_Loss_Price > res.Entry_Price
         then
            res.Stop_Loss_Price := last_res.Stop_Loss_Price;
         end if;
      end if;

      pragma
        Assert
          (conf.Take_Profit_Multiplier = 0.0
             or else res.Take_Profit_Price >= res.Entry_Price);
      pragma
        Assert
          (conf.Stop_Loss_Multiplier = 0.0
             or else res.Entry_Price >= res.Stop_Loss_Price);
   end Calc_TPSL_Prices;

   procedure Trigger_Stop_Loss (res : in out Kernel_Element'Class) is
   begin
      res.Signal := 0;
      res.Trigger := -1;
      res.Stop_Losses := res.Stop_Losses + 1;
   end Trigger_Stop_Loss;

   procedure Trigger_Take_Profit (res : in out Kernel_Element'Class) is
   begin
      res.Signal := 0;
      res.Trigger := -1;
      res.Take_Profits := res.Take_Profits + 1;
   end Trigger_Take_Profit;

   procedure Process_TPSL_Exits
     (res  : in out Kernel_Element'Class;
      curr : Common.Keyed_Lane;
      conf : Scenario_Config'Class)
   is
      tp_threshold : Long_Float;
      tp_price     : Long_Float;
      sl_threshold : Long_Float;
      sl_price     : Long_Float;
   begin
      if conf.Is_Quasi and then conf.Exit_Behavior = Common.TPSL_Self_Managed
      then
         tp_threshold := curr (Common.Bid_Open);
         tp_price := curr (Common.Bid_Open);
         sl_threshold := curr (Common.Bid_Open);
         sl_price := curr (Common.Bid_Open);
      elsif not conf.Is_Quasi
        and then conf.Exit_Behavior = Common.TPSL_Self_Managed
      then
         tp_threshold := curr (Common.Bid_Close);
         tp_price := curr (Common.Bid_Close);
         sl_threshold := curr (Common.Bid_Close);
         sl_price := curr (Common.Bid_Close);
      else
         tp_threshold := curr (Common.Bid_High);
         tp_price := res.Take_Profit_Price;
         sl_threshold := curr (Common.Bid_Low);
         sl_price := res.Stop_Loss_Price;
      end if;

      if conf.Stop_Loss_Multiplier /= 0.0
        and then res.Stop_Loss_Price > sl_threshold
      then
         res.Trigger_Stop_Loss;
         res.Exit_Price := sl_price;
      elsif conf.Take_Profit_Multiplier /= 0.0
        and then res.Take_Profit_Price < tp_threshold
      then
         res.Trigger_Take_Profit;
         res.Exit_Price := tp_price;
      end if;
   end Process_TPSL_Exits;

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
      if res.Exit_Value >= 0.0 then
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

      res           : Kernel_Element := results (index);
      last_res      : Kernel_Element := results (index - 1);
      last_last_res : constant Kernel_Element := results (index - 2);

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

      --  if quasi and the previous candle is an open and this candle
      --  is a close then erase and bail
      if res.Trigger = -1 and then last_res.Trigger = 1 and then conf.Is_Quasi
      then
         last_res.Reset (last_last_res);
         res.Reset (last_res);
         results (index) := res;
         results (index - 1) := last_res;

         return;
      end if;

      --  prepare the result with the previous result totals
      res.Carry_Over_Totals (last_res);

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
         --  pin the entry to the ask close at this tick
         res.Entry_Price := curr (Common.Ask_Close);
         res.Entries := res.Entries + 1;

         res.Calc_TPSL_Prices
           (last_res => last_res, curr => curr, conf => conf);

         --  if volatilty is not acceptable then do not enter a trade
         if conf.Should_Screen_ATR
           and then curr (Common.Ask_Close) - curr (Common.Bid_Close)
                    > Long_Float (conf.Stop_Loss_Multiplier)
                      * curr (Common.ATR)
         then
            res.Reset (last_res);
         end if;

         results (index) := res;
         return;

      elsif res.Trigger = -1 and then res.Signal = 0 then
         --  wma cross so set the exit prices
         res.Exit_Price := bid_exit_price;
         res.Crosses := res.Crosses + 1;

      end if;

      --  carry over the prices to continue current open position and or
      --  get ready to exit
      res.Carry_Over_Prices (last_res);

      --  ensure that the state machine is valid only allowed states at this
      --  point are:
      --  signal = 1 trigger = 0
      --  signal = 0 trigger = -1
      pragma
        Assert
          (last_res.Signal = 1
             and then ((res.Trigger = -1 and then res.Signal = 0)
                       or else (res.Trigger = 0 and then res.Signal = 1)));
      pragma Assert (res.Entries > 0);
      pragma Assert (res.Entry_Price /= 0.0);

      --  process TPSL exits unless it is quasi and already exited
      --  if is quasi and trigger = 0 then true
      --  if not quasi then true
      if (conf.Is_Quasi and then res.Trigger = 0) or else not conf.Is_Quasi
      then
         res.Process_TPSL_Exits (curr, conf);
      end if;

      --  update the exit value and totals or positions
      if res.Trigger = -1 then
         res.Update_Exit_Totals;
         res.Update_Wins_Losses;
         res.Update_Min_Max_Totals (last_res);
      else
         res.Update_Position (bid_exit_price, curr (Common.Ask_Close));

         --  if not default then recalculate tpsl
         if conf.Exit_Behavior /= Common.TPSL_Default then
            res.Calc_TPSL_Prices
              (last_res => last_res, curr => curr, conf => conf);
         end if;
      end if;

      results (index) := res;

   end Kernel;

end Kernel;
