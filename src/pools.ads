pragma Ada_2022;
with Core;

package Pools
is
   type Swim_Lane is array (Positive range <>) of Long_Float;
   type Pool is
     array (Core.Column_Key range Core.Column_Key'Range) of Swim_Lane;

   function Make_Pool
     (ex_candles : Core.Candles; time_interval_period : Positive) return Pool;

end Pools;
