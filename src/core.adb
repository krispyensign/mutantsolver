pragma Ada_2022;

package body Core is

   function Make_HA_Candle
     (current_candle : Candle'Class; previous_candle : Candle'Class)
      return HA_Candle is
   begin
      return
        (Volume    => current_candle.Volume,
         Complete  => current_candle.Complete,
         Ask_Open  =>
           (previous_candle.Ask_Open + previous_candle.Ask_Close) / 2.0,
         Ask_High  =>
           Float'Max
             (Float'Max (current_candle.Ask_Open, current_candle.Ask_Close),
              current_candle.Ask_High),
         Ask_Low   =>
           Float'Max
             (Float'Max (current_candle.Ask_Open, current_candle.Ask_Close),
              current_candle.Ask_Low),
         Ask_Close =>
           (current_candle.Ask_Open + current_candle.Ask_High
            + current_candle.Ask_Low
            + current_candle.Ask_Close)
           / 4.0,
         Mid_Open  =>
           (previous_candle.Mid_Open + previous_candle.Mid_Close) / 2.0,
         Mid_High  =>
           Float'Max
             (Float'Max (current_candle.Mid_Open, current_candle.Mid_Close),
              current_candle.Mid_High),
         Mid_Low   =>
           Float'Max
             (Float'Max (current_candle.Mid_Open, current_candle.Mid_Close),
              current_candle.Mid_Low),
         Mid_Close =>
           (current_candle.Mid_Open + current_candle.Mid_High
            + current_candle.Mid_Low
            + current_candle.Mid_Close)
           / 4.0,
         Bid_Open  =>
           (previous_candle.Bid_Open + previous_candle.Bid_Close) / 2.0,
         Bid_High  =>
           Float'Max
             (Float'Max (current_candle.Bid_Open, current_candle.Bid_Close),
              current_candle.Bid_High),
         Bid_Low   =>
           Float'Max
             (Float'Max (current_candle.Bid_Open, current_candle.Bid_Close),
              current_candle.Bid_Low),
         Bid_Close =>
           (current_candle.Bid_Open + current_candle.Bid_High
            + current_candle.Bid_Low
            + current_candle.Bid_Close)
           / 4.0,
         Time      => current_candle.Time);
   end Make_HA_Candle;

end Core;
