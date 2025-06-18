pragma Ada_2022;

with Config;
with TOML;
with TOML.File_IO;
with Core;
with Oanda_Exchange;

procedure Mutantsolver is

   result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda  : constant Config.Oanda_Access := Config.Load_Oanda (result);
   chart  : constant Config.Chart_Config := Config.Load_Chart_Config (result);

   --  construct URL to retrieve candles
   count : constant Integer := (chart.Train_Set_Size + chart.Sample_Set_Size);

   fetched_candles : Core.Candles_Frame (1 .. count);
begin
   fetched_candles := Oanda_Exchange.Fetch_Candles (oanda, chart);
end Mutantsolver;
