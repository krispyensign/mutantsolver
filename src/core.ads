pragma Ada_2022;
with Ada.Calendar;
with Common;

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

   type Scenario_Result is tagged record
      Entry_Price       : Long_Float;
      Exit_Price        : Long_Float;
      Position          : Long_Float;
      Signal            : Signal_T;
      Trigger           : Trigger_T;
      Take_Profit_Price : Long_Float;
      Stop_Loss_Price   : Long_Float;
      Running_Total     : Long_Float;
      Exit_Value : Long_Float;
      Exit_Total : Long_Float;
   end record;

   type Scenario is record
      Is_Quasi               : Boolean;
      Num_Digits             : Positive;
      Take_Profit_Multiplier : Float;
      Stop_Loss_Multiplier   : Float;
      Precision              : Positive;
      Entry_Key              : Common.Pool_Key;
      Exit_Key               : Common.Pool_Key;
      WMA_Source_Key         : Common.Pool_Key;
   end record;

   procedure Reset (res : in out Scenario_Result'Class);
   procedure Set_Prices
     (res : in out Scenario_Result'Class; last_res : Scenario_Result'Class);

   function Make_HA_Candle
     (current_candle : Candle_Base'Class; previous_candle : Candle_Base'Class)
      return HA_Candle;

   function Make_HA_Candles (in_candles : Candles) return HA_Candles;

end Core;
