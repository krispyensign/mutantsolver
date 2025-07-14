pragma Ada_2022;

package body Core is

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
