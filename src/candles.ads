with Config; use Config;
with Ada.Calendar;
with GNATCOLL.JSON;

package candles is
   package calendar renames Ada.Calendar;
   package json renames GNATCOLL.JSON;

   type Candle is record
      Complete  : Boolean;
      Bid_Open  : Float;
      Bid_High  : Float;
      Bid_Low   : Float;
      Bid_Close : Float;

      Mid_Open  : Float;
      Mid_High  : Float;
      Mid_Low   : Float;
      Mid_Close : Float;

      Ask_Open  : Float;
      Ask_High  : Float;
      Ask_Low   : Float;
      Ask_Close : Float;

      HA_Bid_Open  : Float;
      HA_Bid_High  : Float;
      HA_Bid_Low   : Float;
      HA_Bid_Close : Float;

      HA_Mid_Open  : Float;
      HA_Mid_High  : Float;
      HA_Mid_Low   : Float;
      HA_Mid_Close : Float;

      HA_Ask_Open  : Float;
      HA_Ask_High  : Float;
      HA_Ask_Low   : Float;
      HA_Ask_Close : Float;

      Volume : Integer;
      Time   : calendar.Time;
   end record;

   type Candles_Frame is array (Positive range <>) of Candle;

   function Construct_URL
     (oanda : Oanda_Access; chart : Chart_Config) return String;

   function Make_Candle
     (current_candle : json.JSON_Value; previous_candle : Candle)
      return Candle;

   function Make_Candle (current_candle : json.JSON_Value) return Candle;

   function Fetch_Candles
     (oanda : Oanda_Access; chart : Chart_Config) return Candles_Frame;

end candles;
