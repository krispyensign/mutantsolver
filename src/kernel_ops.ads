pragma Ada_2022;

with Common;
with Kernel;
with Config;

package Kernel_Ops is
   type Operation_Result is record
      best_scenario_report : Kernel.Scenario_Report;
      last_scenario_report : Kernel.Scenario_Report;
      total_count          : Natural := 0;
      total_reported       : Natural := 0;
      total_found          : Natural := 0;
   end record;

   function Find_Max
     (p : Common.Row_Pool; chart : Config.Chart_Config)
      return Operation_Result;

end Kernel_Ops;
