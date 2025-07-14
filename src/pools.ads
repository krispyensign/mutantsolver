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

   function Make_Row_Pool (p : Pool) return Common.Row_Pool;

end Pools;
