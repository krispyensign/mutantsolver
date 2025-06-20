pragma Ada_2022;
with Ada.Calendar;

package Core is
   package calendar renames Ada.Calendar;

   type Candle_Base is tagged record
      Complete  : Boolean;
      Time      : calendar.Time;
      Volume    : Integer;
      Bid_Open  : Long_Float;
      Bid_High  : Long_Float;
      Bid_Low   : Long_Float;
      Bid_Close : Long_Float;

      Mid_Open  : Long_Float;
      Mid_High  : Long_Float;
      Mid_Low   : Long_Float;
      Mid_Close : Long_Float;

      Ask_Open  : Long_Float;
      Ask_High  : Long_Float;
      Ask_Low   : Long_Float;
      Ask_Close : Long_Float;
   end record;

   type Candle is new Candle_Base with null record;
   type Candles_Frame is array (Positive range <>) of Candle;

   type HA_Candle is new Candle_Base with null record;
   type HA_Candle_Frame is array (Positive range <>) of HA_Candle;
   function Make_HA_Candle
     (current_candle : Candle'Class; previous_candle : Candle'Class)
      return HA_Candle;

   type WMA_Candle is new Candle_Base with null record;
   type WMA_HA_Candle is new Candle_Base with null record;

end Core;
