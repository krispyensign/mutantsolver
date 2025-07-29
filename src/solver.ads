pragma Ada_2022;

with Common;
with Core;
with Kernel;
with Config;
with Ada.Real_Time;

package Solver is
   type Operation_Result is record
      Best_Scenario_Ratio  : Float := 0.0;
      Best_Scenario_Config : Kernel.Scenario_Config;
      Best_Scenario_Result : Kernel.Kernel_Element;
      Total_Count          : Natural := 0;
      Total_Reported       : Natural := 0;
      Total_Found          : Natural := 0;
   end record;

   function Find_Max
     (p : Common.Row_Pool; chart : Config.Chart_Config)
      return Operation_Result;

   function Find_Max_TP_SL
     (p     : Common.Row_Pool;
      chart : Config.Chart_Config;
      conf  : Kernel.Scenario_Config) return Operation_Result;

   type Offline_Solve_Result is record
      Best_Scenario_Config     : Kernel.Scenario_Config;
      Best_Scenario_Result     : Kernel.Kernel_Element;
      Total_Count              : Natural := 0;
      Total_Reported           : Natural := 0;
      Total_Found              : Natural := 0;
      Refined_Config           : Kernel.Scenario_Config;
      Refined_Result           : Kernel.Kernel_Element;
      ZK_Online_Result         : Kernel.Kernel_Element;
      ZK_Refined_Online_Result : Kernel.Kernel_Element;
      PK_Online_Result         : Kernel.Kernel_Element;
      Total_Time_Duration      : Ada.Real_Time.Time_Span;
      Throughput               : Float := 0.0;
   end record;

   function Offline_Solve
     (ex_candles : Core.Candles; chart : Config.Chart_Config)
      return Offline_Solve_Result;

end Solver;
