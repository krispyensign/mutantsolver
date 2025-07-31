pragma Ada_2022;
with Config;
with Ada.Calendar;
with GNATCOLL.JSON;
with Core;

package Oanda_Exchange is
   package calendar renames Ada.Calendar;
   package json renames GNATCOLL.JSON;

   function Fetch_Candles
     (oanda      : Config.Oanda_Access;
      chart      : Config.Chart_Config;
      date_index : Natural := 0) return Core.Candles
   with Pre => chart.Offline_Set_Size + chart.Online_Set_Size <= 5000;

private
   function Fetch_Candle_Data
     (token : String; constructed_url : String) return String;

   function Construct_URL
     (oanda : Config.Oanda_Access; chart : Config.Chart_Config) return String;

   function Construct_URL
     (oanda   : Config.Oanda_Access;
      chart   : Config.Chart_Config;
      to_time : calendar.Time) return String;

   function Make_Candle (current_candle : json.JSON_Value) return Core.Candle;

end Oanda_Exchange;
