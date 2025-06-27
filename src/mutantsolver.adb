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
   result         : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda          : constant Config.Oanda_Access := Config.Load_Oanda (result);
   chart          : constant Config.Chart_Config :=
     Config.Load_Chart_Config (result);
   count          : constant Integer :=
     (chart.Offline_Set_Size + chart.Online_Set_Size);
   --  fetch the candles and allocate candles on the stack
   ex_candles     : Core.Candles (1 .. count);
   --  initialize the Technical analysis library TA-Lib
   ta_result      : constant Integer := TA.TA_Initialize;
   --  convert candle records to pool object
   package full_p is new Pools (Count => count);
   full_data_pool : full_p.Pool;

   package offline_p is new Pools (Count => chart.Offline_Set_Size);
   offline_data_pool : offline_p.Pool;

   package online_p is new Pools (Count => chart.Online_Set_Size);
   online_data_pool : online_p.Pool;

   package tp_sl_offline_p is new
     Pools (Count => chart.TP_SL_Offline_Set_Size);
   tp_sl_offline_data_pool : tp_sl_offline_p.Pool;

begin
   --  fetch the candles and allocate candles on the stack
   ex_candles := Oanda_Exchange.Fetch_Candles (oanda, chart);
   --  apply the heiken ashi transform
   full_data_pool := full_p.Make_Pool (ex_candles, chart.Time_Period_Interval);

   offline_data_pool :=
     full_data_pool (Core.Column_Key'First .. Core.Column_Key'Last)
       (1 .. chart.Offline_Set_Size);

end Mutantsolver;
