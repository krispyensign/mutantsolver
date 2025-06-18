pragma Ada_2022;

with Config; use Config;
with TOML;
with TOML.File_IO;
with candles;

procedure Mutantsolver is

   result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda  : constant Oanda_Access := Load_Oanda (result);
   chart  : constant Chart_Config := Load_Chart_Config (result);

   --  construct URL to retrieve candles
   count : constant Integer := (chart.Train_Set_Size + chart.Sample_Set_Size);

   fetched_candles : candles.Candles_Frame (1 .. count);
begin
   fetched_candles := candles.Fetch_Candles (oanda, chart);
end Mutantsolver;
