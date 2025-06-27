pragma Ada_2022;
with TA;

package body Pools is

   function Make_Pool
     (ex_candles : Core.Candles; time_interval_period : Positive) return Pool
   is
      --  apply the heiken ashi transform
      ha_candles     : constant Core.HA_Candles (1 .. count) :=
        [for i in ex_candles'Range
         => (if i = 1 then Core.Make_HA_Candle (ex_candles (1), ex_candles (1))
             else Core.Make_HA_Candle (ex_candles (i), ex_candles (i - 1)))];
      --  convert candle records to pool object
      full_data_pool : Pool :=
        [Core.Ask_Open     => [for i in 1 .. Count => ex_candles (i).Ask_Open],
         Core.Ask_High     => [for i in 1 .. Count => ex_candles (i).Ask_High],
         Core.Ask_Low      => [for i in 1 .. Count => ex_candles (i).Ask_Low],
         Core.Ask_Close    =>
           [for i in 1 .. Count => ex_candles (i).Ask_Close],
         Core.Mid_Open     => [for i in 1 .. Count => ex_candles (i).Mid_Open],
         Core.Mid_High     => [for i in 1 .. Count => ex_candles (i).Mid_High],
         Core.Mid_Low      => [for i in 1 .. Count => ex_candles (i).Mid_Low],
         Core.Mid_Close    =>
           [for i in 1 .. Count => ex_candles (i).Mid_Close],
         Core.Bid_Open     => [for i in 1 .. Count => ex_candles (i).Bid_Open],
         Core.Bid_High     => [for i in 1 .. Count => ex_candles (i).Bid_High],
         Core.Bid_Low      => [for i in 1 .. Count => ex_candles (i).Bid_Low],
         Core.Bid_Close    =>
           [for i in 1 .. Count => ex_candles (i).Bid_Close],
         Core.HA_Ask_Open  => [for i in 1 .. Count => ha_candles (i).Ask_Open],
         Core.HA_Ask_High  => [for i in 1 .. Count => ha_candles (i).Ask_High],
         Core.HA_Ask_Low   => [for i in 1 .. Count => ha_candles (i).Ask_Low],
         Core.HA_Ask_Close =>
           [for i in 1 .. Count => ha_candles (i).Ask_Close],
         Core.HA_Mid_Open  => [for i in 1 .. Count => ha_candles (i).Mid_Open],
         Core.HA_Mid_High  => [for i in 1 .. Count => ha_candles (i).Mid_High],
         Core.HA_Mid_Low   => [for i in 1 .. Count => ha_candles (i).Mid_Low],
         Core.HA_Mid_Close =>
           [for i in 1 .. Count => ha_candles (i).Mid_Close],
         Core.HA_Bid_Open  => [for i in 1 .. Count => ha_candles (i).Bid_Open],
         Core.HA_Bid_High  => [for i in 1 .. Count => ha_candles (i).Bid_High],
         Core.HA_Bid_Low   => [for i in 1 .. Count => ha_candles (i).Bid_Low],
         Core.HA_Bid_Close =>
           [for i in 1 .. Count => ha_candles (i).Bid_Close],
         others            => [for i in 1 .. Count => 0.0]];

   begin
      --  Calculate the ATR
      full_data_pool (Core.ATR) :=
        Swim_Lane
          (TA.Calc_TA_ATR
             (in_high     => Core.Real_Array (full_data_pool (Core.Mid_High)),
              in_low      => Core.Real_Array (full_data_pool (Core.Mid_Low)),
              in_close    => Core.Real_Array (full_data_pool (Core.Mid_Close)),
              time_period => time_interval_period));

      full_data_pool (Core.WMA_Ask_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Ask_Open)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_Ask_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Ask_High)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_Ask_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Ask_Low)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_Ask_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Ask_Close)),
              time_period => time_interval_period));

      full_data_pool (Core.WMA_Mid_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Mid_Open)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_Mid_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Mid_High)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_Mid_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Mid_Low)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_Mid_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Mid_Close)),
              time_period => time_interval_period));

      full_data_pool (Core.WMA_Bid_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Bid_Open)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_Bid_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Bid_High)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_Bid_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Bid_Low)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_Bid_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     => Core.Real_Array (full_data_pool (Core.Bid_Close)),
              time_period => time_interval_period));

      full_data_pool (Core.WMA_HA_Ask_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Ask_Open)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_HA_Ask_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Ask_High)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_HA_Ask_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Ask_Low)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_HA_Ask_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Ask_Close)),
              time_period => time_interval_period));

      full_data_pool (Core.WMA_HA_Mid_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Mid_Open)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_HA_Mid_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Mid_High)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_HA_Mid_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Mid_Low)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_HA_Mid_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Mid_Close)),
              time_period => time_interval_period));

      full_data_pool (Core.WMA_HA_Bid_Open) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Bid_Open)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_HA_Bid_High) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Bid_High)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_HA_Bid_Low) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Bid_Low)),
              time_period => time_interval_period));
      full_data_pool (Core.WMA_HA_Bid_Close) :=
        Swim_Lane
          (TA.Calc_TA_WMA
             (in_real     =>
                Core.Real_Array (full_data_pool (Core.HA_Bid_Close)),
              time_period => time_interval_period));

      return full_data_pool;
   end Make_Pool;
end Pools;
