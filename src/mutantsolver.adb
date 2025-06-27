pragma Ada_2022;

with Config;
with TOML;
with TOML.File_IO;
with Oanda_Exchange;
with TA;
with Core;
with Pools;

procedure Mutantsolver is
   --  load the configs from the toml files
   result : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda  : constant Config.Oanda_Access := Config.Load_Oanda (result);
   chart  : constant Config.Chart_Config := Config.Load_Chart_Config (result);
   count  : constant Integer := (chart.Train_Set_Size + chart.Sample_Set_Size);

   --  fetch the candles and allocate candles on the stack
   ex_candles : constant Core.Candles (1 .. count) :=
     Oanda_Exchange.Fetch_Candles (oanda, chart);

   --  apply the heiken ashi transform
   ha_candles : constant Core.HA_Candles (1 .. count) :=
     [for i in ex_candles'Range
      => (if i = 1 then Core.Make_HA_Candle (ex_candles (1), ex_candles (1))
          else Core.Make_HA_Candle (ex_candles (i), ex_candles (i - 1)))];

   --  initialize the Technical analysis library TA-Lib
   ta_result : Integer := TA.TA_Initialize;

   --  convert candle records to pool object
   package p is new Pools (Count => count);
   full_data_pool : p.Pool :=
     p.Make_Pool (ex_candles, ha_candles, chart.Time_Period_Interval);

begin
   null;

end Mutantsolver;
