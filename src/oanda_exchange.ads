pragma Ada_2022;
with Config;
with Ada.Calendar;
with GNATCOLL.JSON;
with Core;

package Oanda_Exchange is
   package calendar renames Ada.Calendar;
   package json renames GNATCOLL.JSON;

   function Construct_URL
     (oanda : Config.Oanda_Access; chart : Config.Chart_Config) return String;

   function Make_Candle (current_candle : json.JSON_Value) return Core.Candle;

   function Fetch_Candles
     (oanda : Config.Oanda_Access; chart : Config.Chart_Config)
      return Core.Candles
      with Post => Fetch_Candles'Result'Length > 0;

   function Fetch_Candle_Data
     (token : String; constructed_url : String) return json.JSON_Array;
end Oanda_Exchange;
