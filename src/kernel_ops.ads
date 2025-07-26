pragma Ada_2022;

with Common;
with Kernel;
with Config;

package Kernel_Ops is
   type Scenario_Report is record
      Config         : Kernel.Scenario_Config;
      Wins           : Natural := 0;
      Losses         : Natural := 0;
      Take_Profits   : Natural := 0;
      Stop_Losses    : Natural := 0;
      Crosses        : Natural := 0;
      Entries        : Natural := 0;
      Max_Exit_Total : Long_Float := Long_Float'First;
      Min_Exit_Total : Long_Float := Long_Float'Last;
      Ratio          : Float := 0.0;
      Final_Total    : Long_Float := Long_Float'First;
   end record;

   type Operation_Result is record
      best_scenario_report : Scenario_Report;
      last_scenario_report : Scenario_Report;
      total_count          : Natural := 0;
      total_reported       : Natural := 0;
      total_found          : Natural := 0;
   end record;

   function Find_Max
     (p : Common.Row_Pool; chart : Config.Chart_Config)
      return Operation_Result;

   function Find_Max_TP_SL
     (p     : Common.Row_Pool;
      chart : Config.Chart_Config;
      conf  : Kernel.Scenario_Config) return Operation_Result;

end Kernel_Ops;
