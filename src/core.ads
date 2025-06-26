pragma Ada_2022;
with Ada.Calendar;
with common;

package Core is
   package calendar renames Ada.Calendar;

   type Real_Array is array (Positive range <>) of Long_Float;
   type Real_Array_Ptr is not null access all Real_Array;

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

   type Pool_T is array (Column_Key range <>, Positive range <>) of Long_Float ;

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

   function Make_HA_Candle
     (current_candle : Candle_Base'Class; previous_candle : Candle_Base'Class)
      return HA_Candle;

   type Metadata is record
      Start_Time  : calendar.Time;
      End_Time    : calendar.Time;
      Granularity : Duration;
      Count       : Integer;
   end record;

end Core;
