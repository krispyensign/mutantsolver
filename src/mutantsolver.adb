pragma Ada_2022;
pragma Extensions_Allowed (On);

with Config;
with TOML;
with TOML.File_IO;
with Oanda_Exchange;
with Core;
with Solver;
with Ada.Text_IO;
with Ada.Real_Time;
with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Util.Dates.ISO8601;

procedure Mutantsolver is
   package io renames Ada.Text_IO;
   package cmd renames Ada.Command_Line;
   package ubo renames Ada.Strings.Unbounded;
   package format renames Ada.Calendar.Formatting;

   procedure Print_Results
     (result : Solver.Offline_Solve_Result; chart : Config.Chart_Config) is
   begin
      io.Put_Line
        ("duration:"
         & result.Total_Time_Duration'Image
         & " throughput: "
         & result.Throughput'Image);
      io.Put_Line
        ("config ins: "
         & chart.Instrument
         & " satr:"
         & chart.Should_Screen_ATR'Image
         & " tp:"
         & chart.TPSL_Behavior'Image);
      io.Put_Line
        ("offline rfet:"
         & result.ZK_Refined_Offline_Result.Exit_Total'Image
         & " et:"
         & result.Best_Scenario_Result.Exit_Total'Image);
      io.Put_Line
        ("online zet:"
         & result.ZK_Online_Result.Exit_Total'Image
         & " ret:"
         & result.ZK_Refined_Online_Result.Exit_Total'Image
         & " pet:"
         & result.PK_Online_Result.Exit_Total'Image);
   end Print_Results;

   procedure Log_ex_candles
     (ex_candles : Core.Candles; chart : Config.Chart_Config) is
   begin
      io.Put_Line
        ("t1:"
         & format.Local_Image (ex_candles (1).Time)
         & " t2:"
         & format.Local_Image (ex_candles (chart.Offline_Set_Size).Time)
         & " t3:"
         & format.Local_Image
             (ex_candles ((chart.Offline_Set_Size + chart.Online_Set_Size))
                .Time));
   end Log_ex_candles;

begin
   --  look for the config file first
   filename : ubo.Unbounded_String :=
     ubo.To_Unbounded_String ("local_config.toml");
   for Next in 1 .. cmd.Argument_Count loop
      if cmd.Argument (Next) = "--config" then
         filename := ubo.To_Unbounded_String (cmd.Argument (Next + 1));
      end if;
   end loop;

   --  load the configs from the toml files
   load_result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File (filename.To_String);
   sys_conf : constant Config.System_Config :=
     Config.Load_System (load_result);
   oanda : constant Config.Oanda_Access := Config.Load_Oanda (load_result);

   chart : Config.Chart_Config := Config.Load_Chart_Config (load_result);
   count : constant Positive :=
     (chart.Offline_Set_Size + chart.Online_Set_Size);

   --  then override config
   for Next in 1 .. cmd.Argument_Count loop
      --  configure the instrument
      if cmd.Argument (Next) = "--instrument" then
         chart.Instrument := cmd.Argument (Next + 1);
      end if;

      --  configure a one off date
      if cmd.Argument (Next) = "--date" then
         chart.Dates := [Util.Dates.ISO8601.Value (cmd.Argument (Next + 1))];
      end if;
   end loop;

   --  loop through all the dates and sum up the exit totals
   final_zk : Long_Float := 0.0;
   final_pk : Long_Float := 0.0;
   final_rzk : Long_Float := 0.0;
   start_time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   for i in chart.Dates'Range loop
      --  fetch the candles
      ex_candles : constant Core.Candles (1 .. count) :=
        Oanda_Exchange.Fetch_Candles (oanda, chart, sys_conf, i);
      Log_ex_candles (ex_candles, chart);

      --  run the solver
      result : constant Solver.Offline_Solve_Result :=
        Solver.Offline_Solve (ex_candles => ex_candles, chart => chart);

      --  print the results
      Print_Results (result, chart);

      --  update and print the final results
      final_zk := final_zk + result.ZK_Online_Result.Exit_Total;
      final_pk := final_pk + result.PK_Online_Result.Exit_Total;
      final_rzk := final_rzk + result.ZK_Refined_Online_Result.Exit_Total;
      io.Put_Line
        ("running zet:"
         & final_zk'Image
         & " ret:"
         & final_rzk'Image
         & " pet:"
         & final_pk'Image);

   end loop;

   --  log the total time spent
   end_time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   total_time_duration : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time."-" (end_time, start_time);
   io.Put_Line ("total duration:" & total_time_duration'Image);

end Mutantsolver;
