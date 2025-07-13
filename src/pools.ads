pragma Ada_2022;

with Common;
with Core;

generic
   Count : Positive;
package Pools is

   type Swim_Lane is array (Positive range 1 .. Count) of Long_Float;
   type Pool is
     array (Common.Pool_Key range Common.Pool_Key'Range) of Swim_Lane;

   function Make_Pool
     (ex_candles : Core.Candles; time_interval_period : Positive) return Pool;

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
      results                : in out Core.Scenario_Result);

end Pools;
