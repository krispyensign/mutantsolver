pragma Ada_2022;

package body Core is

   function Make_HA_Candle
     (current_candle : Candle; previous_candle : Candle) return Candle is
   begin
      return
        (Volume       => current_candle.Volume,
         Complete     => current_candle.Complete,
         Ask_Open     => current_candle.Ask_Open,
         Ask_High     => current_candle.Ask_High,
         Ask_Low      => current_candle.Ask_Low,
         Ask_Close    => current_candle.Ask_Close,
         Mid_Open     => current_candle.Mid_Open,
         Mid_High     => current_candle.Mid_High,
         Mid_Low      => current_candle.Mid_Low,
         Mid_Close    => current_candle.Mid_Close,
         Bid_Open     => current_candle.Bid_Open,
         Bid_High     => current_candle.Bid_High,
         Bid_Low      => current_candle.Bid_Low,
         Bid_Close    => current_candle.Bid_Close,
         HA_Ask_Open  =>
           (previous_candle.Ask_Open + previous_candle.Ask_Close) / 2.0,
         HA_Ask_High  =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Ask_Open, current_candle.Ask_Close),
              current_candle.Ask_High),
         HA_Ask_Low   =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Ask_Open, current_candle.Ask_Close),
              current_candle.Ask_Low),
         HA_Ask_Close =>
           (current_candle.Ask_Open + current_candle.Ask_High
            + current_candle.Ask_Low
            + current_candle.Ask_Close)
           / 4.0,
         HA_Mid_Open  =>
           (previous_candle.Mid_Open + previous_candle.Mid_Close) / 2.0,
         HA_Mid_High  =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Mid_Open, current_candle.Mid_Close),
              current_candle.Mid_High),
         HA_Mid_Low   =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Mid_Open, current_candle.Mid_Close),
              current_candle.Mid_Low),
         HA_Mid_Close =>
           (current_candle.Mid_Open + current_candle.Mid_High
            + current_candle.Mid_Low
            + current_candle.Mid_Close)
           / 4.0,
         HA_Bid_Open  =>
           (previous_candle.Bid_Open + previous_candle.Bid_Close) / 2.0,
         HA_Bid_High  =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Bid_Open, current_candle.Bid_Close),
              current_candle.Bid_High),
         HA_Bid_Low   =>
           Long_Float'Max
             (Long_Float'Max
                (current_candle.Bid_Open, current_candle.Bid_Close),
              current_candle.Bid_Low),
         HA_Bid_Close =>
           (current_candle.Bid_Open + current_candle.Bid_High
            + current_candle.Bid_Low
            + current_candle.Bid_Close)
           / 4.0,
         Time         => current_candle.Time,
         others       => 0.0);
   end Make_HA_Candle;

end Core;
