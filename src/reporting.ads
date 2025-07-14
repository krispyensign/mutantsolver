pragma Ada_2022;

with Core;

package Reporting is
   task Reporting is
      entry Update_Scenario (scenario_report : out Core.Scenario_Report);
   end Reporting;

end Reporting;
