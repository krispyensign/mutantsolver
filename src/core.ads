pragma Ada_2022;
with Ada.Calendar;

package Core is
   package calendar renames Ada.Calendar;

   type Column_Key is
     (Time,
      Volume,
      ATR,
      Bid_Open,
      Bid_High,
      Bid_Low,
      Bid_Close,
      Mid_Open,
      Mid_High,
      Mid_Low,
      Mid_Close,
      Ask_Open,
      Ask_High,
      Ask_Low,
      Ask_Close,
      HA_Bid_Open,
      HA_Bid_High,
      HA_Bid_Low,
      HA_Bid_Close,
      HA_Mid_Open,
      HA_Mid_High,
      HA_Mid_Low,
      HA_Mid_Close,
      HA_Ask_Open,
      HA_Ask_High,
      HA_Ask_Low,
      HA_Ask_Close,
      WMA_Bid_Open,
      WMA_Bid_High,
      WMA_Bid_Low,
      WMA_Bid_Close,
      WMA_Mid_Open,
      WMA_Mid_High,
      WMA_Mid_Low,
      WMA_Mid_Close,
      WMA_Ask_Open,
      WMA_Ask_High,
      WMA_Ask_Low,
      WMA_Ask_Close,
      WMA_HA_Bid_Open,
      WMA_HA_Bid_High,
      WMA_HA_Bid_Low,
      WMA_HA_Bid_Close,
      WMA_HA_Mid_Open,
      WMA_HA_Mid_High,
      WMA_HA_Mid_Low,
      WMA_HA_Mid_Close,
      WMA_HA_Ask_Open,
      WMA_HA_Ask_High,
      WMA_HA_Ask_Low,
      WMA_HA_Ask_Close);

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

   type Real_Array is array (Positive range <>) of Long_Float;
   type Real_Array_Ptr is not null access all Real_Array;
   type Candles is array (Positive range <>) of Candle;
   type HA_Candles is array (Positive range <>) of HA_Candle;

   subtype Signal_T is Integer range 0 .. 1;
   subtype Trigger_T is Integer range -1 .. 1;

   type Result is record
      Entry_Value : Long_Float;
      Exit_Value : Long_Float;
      Position : Long_Float;
      Signal : Signal_T;
      Trigger : Trigger_T;
      Take_Profit_Price : Long_Float;
      Stop_Loss_Price : Long_Float;
      Running_Total : Long_Float;
   end record;

   type Scenario is record
      Is_Quasi : Boolean;
      Take_Profit_Multiplier : Float;
      Stop_Loss_Multiplier : Float;
      Precision : Positive;
      Entry_Key : Column_Key;
      Exit_Key : Column_Key;
      WMA_Source_Key : Column_Key;
   end record;

   function Make_HA_Candle
     (current_candle : Candle_Base'Class; previous_candle : Candle_Base'Class)
      return HA_Candle;

   function Make_HA_Candles (in_candles : Candles) return HA_Candles;

end Core;
