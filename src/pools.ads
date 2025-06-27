pragma Ada_2022;
with Core;

generic
   Count : Positive;

package Pools
is
   type Swim_Lane is array (Positive range 1 .. Count) of Long_Float;
   type Pool is
     array (Core.Column_Key
              range Core.Column_Key'First .. Core.Column_Key'Last)
     of Swim_Lane;

   function Make_Pool
     (ex_candles : Core.Candles; time_interval_period : Positive) return Pool;

end Pools;
