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

   --  full_data_pool is a pool that contains all candles
   package full_p is new Pools (Count => count);
   full_data_pool : full_p.Pool;

   --  offline_data_pool is a pool that contains the offline candles that will
   --  be used to solve for optimized entry, exit, source, take profit, and
   --  stop loss components
   package offline_p is new Pools (Count => chart.Offline_Set_Size);
   offline_data_pool : offline_p.Pool;

   --  tp_sl_offline_data_pool is a pool that contains a copied slice of the
   --  offline candles that will be used for solving for optimized take profit
   --  and stop loss multipliers
   package tp_sl_offline_p is new
     Pools (Count => chart.TP_SL_Offline_Set_Size);
   tp_sl_offline_data_pool : tp_sl_offline_p.Pool;

   --  online_data_pool is a pool that contains the online candles that
   --  will not be solved for and are only used for zero knowledge evaluation
   --  of the selected strategy
   package online_p is new Pools (Count => chart.Online_Set_Size);
   online_data_pool : online_p.Pool;



begin
   --  fetch the candles
   ex_candles := Oanda_Exchange.Fetch_Candles (oanda, chart);

   --  populate the data pool from the retrieved candles
   full_data_pool := full_p.Make_Pool (ex_candles, chart.Time_Period_Interval);

   --  partition the offline data pool
   offline_data_pool :=
     [for i in Core.Column_Key'Range
      => offline_p.Swim_Lane
           (full_data_pool (i) (1 .. chart.Offline_Set_Size))];

   --  populate the tp/sl offline data pool
   tp_sl_offline_data_pool :=
     [for i in Core.Column_Key'Range
      => tp_sl_offline_p.Swim_Lane
           (offline_data_pool (i)
              (chart.Offline_Set_Size - chart.TP_SL_Offline_Set_Size + 1
               .. chart.Offline_Set_Size))];

   --  poplate the simulated online data pool for zero knowledge tests
   online_data_pool :=
     [for i in Core.Column_Key'Range
      => online_p.Swim_Lane
           (full_data_pool (i) (chart.Offline_Set_Size + 1 .. count))];

end Mutantsolver;
