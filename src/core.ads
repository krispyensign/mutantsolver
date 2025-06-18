pragma Ada_2022;
with Ada.Calendar;

package Core is
   package calendar renames Ada.Calendar;

   type Candle_Base is tagged record
      Complete  : Boolean;
      Time      : calendar.Time;
      Volume    : Integer;
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
   end record;

   type Candle is new Candle_Base with null record;
   type Candles_Frame is array (Positive range <>) of Candle;

   type HA_Candle is new Candle_Base with null record;
   type HA_Candle_Frame is array (Positive range <>) of HA_Candle;
   function Make_HA_Candle
     (current_candle : Candle'Class; previous_candle : Candle'Class) return HA_Candle;

   type WMA_Candle is new Candle_Base with null record;
   type WMA_HA_Candle is new Candle_Base with null record;

end Core;
