pragma Ada_2022;
with Ada.Text_IO;

package body Reporting is

   package io renames Ada.Text_IO;

   task body Reporting is
      best_scenario_report : Core.Scenario_Report;
      last_scenario_report : Core.Scenario_Report;
      total_found          : Natural := 0;
      total_reported       : Natural := 0;
   begin
      loop
         accept Update_Scenario (scenario_report : out Core.Scenario_Report) do
            last_scenario_report := scenario_report;
            total_reported := total_reported + 1;

            if scenario_report.Final_Total <= 0.0
              or else abs (scenario_report.Max_Exit_Total)
                      <= abs (scenario_report.Min_Exit_Total)
            then
               return;
            end if;

            total_found := total_found + 1;

            if (best_scenario_report.Final_Total = 0.0)
              or else (scenario_report.Ratio >= best_scenario_report.Ratio
                       and then scenario_report.Final_Total
                                >= best_scenario_report.Final_Total)
            then
               best_scenario_report := scenario_report;
               io.Put_Line ("----");
               io.Put_Line (scenario_report.Final_Total'Image);
               io.Put_Line (scenario_report.Ratio'Image);
            end if;
         end Update_Scenario;
      end loop;
   end Reporting;
end Reporting;
