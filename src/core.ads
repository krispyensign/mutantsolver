pragma Ada_2022;
with Ada.Calendar;

package Core is
   package calendar renames Ada.Calendar;

   type Candle_Base is tagged record
      Complete                               : Boolean;
      Time                                   : calendar.Time;
      Volume                                 : Integer;
      Bid_Open, Bid_High, Bid_Low, Bid_Close : Long_Float;
      Mid_Open, Mid_High, Mid_Low, Mid_Close : Long_Float;
      Ask_Open, Ask_High, Ask_Low, Ask_Close : Long_Float;
   end record;

   type Candle is new Candle_Base with null record;
   type HA_Candle is new Candle_Base with null record;

   type Candles is array (Positive range <>) of Candle;
   type HA_Candles is array (Positive range <>) of HA_Candle;

   subtype Signal_T is Integer range 0 .. 1;
   subtype Trigger_T is Integer range -1 .. 1;

   function Make_HA_Candle
     (current_candle : Candle_Base'Class; previous_candle : Candle_Base'Class)
      return HA_Candle;

   function Make_HA_Candles (in_candles : Candles) return HA_Candles;

end Core;
