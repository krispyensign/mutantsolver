pragma Ada_2022;

with Pools;

package body Core is

   function Update_Prices
     (c : Positive; p : Pools.Pool; i : Positive; s : Scenario; num_digits : Positive)
      return Result
   is
      res : Result;
   begin
      res.Take_Profit_Price :=
        p (Ask_Close) (i) + p (ATR) (i) * s.Take_Profit_Multiplier;
      res.Stop_Loss_Price :=
        p (Ask_Close) (i) - p (ATR) (i) * s.Stop_Loss_Multiplier;
      if res.Stop_Loss_Price > p (Bid_Close) (i) then
         res.Stop_Loss_Price := p (Bid_Close) (i) - 10 ** (-num_digits);
      end if;

      return res;
   end Update_Prices;

   procedure Calc_WMA_Signal
     (p          : Pools.Pool;
      i          : Positive;
      s          : Scenario;
      num_digits : Positive;
      last_res   : Result;
      res        : in out Result)
   is
      buy_signal       : Boolean :=
        p (s.Entry_Key) (i) > p (s.WMA_Source_Key) (i);
      prior_buy_signal : Boolean :=
        p (s.Entry_Key) (i - 1) > p (s.WMA_Source_Key) (i - 1);
      exit_signal      : Boolean := p (s.Exit_Key) > p (s.WMA_Source_Key) (i);
   begin
      res.Signal :=
        (if (not prior_buy_signal and buy_signal)
           or (prior_buy_signal and exit_signal)
         then 1
         else 0);
      res.Trigger := res.Signal - last_res.Signal;
   end Calc_WMA_Signal;

   function Make_HA_Candles (in_candles : Candles) return HA_Candles is
      temp_ha_candles : HA_Candles :=
        [for i in 1 .. in_candles'Length
         => (if i = 1 then Core.Make_HA_Candle (in_candles (1), in_candles (1))
             else Core.Make_HA_Candle (in_candles (i), in_candles (i - 1)))];
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
