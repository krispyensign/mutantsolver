pragma Ada_2022;

with Config;
with TOML;
with TOML.File_IO;
with Oanda_Exchange;
with Core;
with Solver;
with Ada.Text_IO;

procedure Mutantsolver is
   package io renames Ada.Text_IO;
   --  load the configs from the toml files
   load_result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda       : constant Config.Oanda_Access :=
     Config.Load_Oanda (load_result);
   chart       : constant Config.Chart_Config :=
     Config.Load_Chart_Config (load_result);
   count       : constant Positive :=
     (chart.Offline_Set_Size + chart.Online_Set_Size);

   --  fetch the candles
   ex_candles : constant Core.Candles (1 .. count) :=
     Oanda_Exchange.Fetch_Candles (oanda, chart);

   result : Solver.Offline_Solve_Result;

begin

   result := Solver.Offline_Solve (ex_candles => ex_candles, chart => chart);

   --  print the configs
   io.Put_Line (result'Image);
end Mutantsolver;
