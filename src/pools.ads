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

   procedure Pin_Prices
     (p   : Pool;
      i   : Positive;
      s   : Core.Scenario;
      res : in out Core.Scenario_Result_Element);

   function Calc_WMA_Signal
     (p        : Pool;
      i        : Positive;
      s        : Core.Scenario;
      last_res : Core.Scenario_Result_Element)
      return Core.Scenario_Result_Element;

   procedure Kernel
     (p       : Pool;
      i       : Positive;
      s       : Core.Scenario;
      results : in out Core.Scenario_Result);

end Pools;
