pragma Ada_2022;

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
with Util.Dates.ISO8601;

procedure Mutantsolver is
   package io renames Ada.Text_IO;
   package cmd renames Ada.Command_Line;

   --  load the configs from the toml files
   load_result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   sys_conf    : constant Config.System_Config :=
     Config.Load_System (load_result);
   oanda       : constant Config.Oanda_Access :=
     Config.Load_Oanda (load_result);
   chart       : Config.Chart_Config :=
     Config.Load_Chart_Config (load_result);
   count       : constant Positive :=
     (chart.Offline_Set_Size + chart.Online_Set_Size);

   ex_candles : Core.Candles (1 .. count);

   result              : Solver.Offline_Solve_Result;
   final_zk            : Long_Float := 0.0;
   final_pk            : Long_Float := 0.0;
   final_rzk           : Long_Float := 0.0;
   start_time          : Ada.Real_Time.Time;
   end_time            : Ada.Real_Time.Time;
   total_time_duration : Ada.Real_Time.Time_Span;

begin
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

   --  start the main loop timer
   start_time := Ada.Real_Time.Clock;

   --  fetch the candles
   for i in chart.Dates'Range loop
      ex_candles := Oanda_Exchange.Fetch_Candles (oanda, chart, sys_conf, i);
      io.Put_Line
        ("t1:"
         & Ada.Calendar.Formatting.Local_Image (ex_candles (1).Time)
         & " t2:"
         & Ada.Calendar.Formatting.Local_Image
             (ex_candles (chart.Offline_Set_Size).Time)
         & " t3:"
         & Ada.Calendar.Formatting.Local_Image (ex_candles (count).Time));
      result :=
        Solver.Offline_Solve (ex_candles => ex_candles, chart => chart);

      --  print the results
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

   end_time := Ada.Real_Time.Clock;
   total_time_duration := Ada.Real_Time."-" (end_time, start_time);

   io.Put_Line ("total duration:" & total_time_duration'Image);
end Mutantsolver;
