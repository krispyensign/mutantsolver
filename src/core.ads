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

   type Scenario_Result_Element is tagged record
      Signal            : Signal_T := 0;
      Trigger           : Trigger_T := 0;
      Entry_Price       : Long_Float := 0.0;
      Exit_Price        : Long_Float := 0.0;
      Position          : Long_Float := 0.0;
      Take_Profit_Price : Long_Float := 0.0;
      Stop_Loss_Price   : Long_Float := 0.0;
      Running_Total     : Long_Float := 0.0;
      Exit_Value        : Long_Float := 0.0;

      Exit_Total     : Long_Float := 0.0;
      Wins           : Natural := 0;
      Losses         : Natural := 0;
      Min_Exit_Total : Long_Float := Long_Float'Last;
      Max_Exit_Total : Long_Float := Long_Float'First;
      Ratio          : Float := 0.0;
   end record;

   type Kernel_Element is record
      WMA_Component_Price   : Long_Float := 0.0;
      Entry_Component_Price : Long_Float := 0.0;
      Exit_Component_Price  : Long_Float := 0.0;

      ATR       : Long_Float := 0.0;
      Ask_Close : Long_Float := 0.0;
      Bid_Open  : Long_Float := 0.0;
      Bid_High  : Long_Float := 0.0;
      Bid_Low   : Long_Float := 0.0;
      Bid_Close : Long_Float := 0.0;
   end record;

   type Scenario_Config is tagged record
      Start_Index            : Positive;
      Is_Quasi               : Boolean := False;
      Num_Digits             : Positive := 5;
      Take_Profit_Multiplier : Float := 0.0;
      Stop_Loss_Multiplier   : Float := 0.0;
      Entry_Key              : Common.Candle_Key := Common.Bid_Open;
      Exit_Key               : Common.Candle_Key := Common.Bid_Open;
      WMA_Source_Key         : Common.WMA_Source_Key := Common.WMA_Bid_Open;
   end record;

   type Scenario_Report is record
      Config         : Scenario_Config;
      Wins           : Natural := 0;
      Losses         : Natural := 0;
      Max_Exit_Total : Long_Float := 0.0;
      Min_Exit_Total : Long_Float := 0.0;
      Ratio          : Float := 0.0;
      Final_Total    : Long_Float := 0.0;
   end record;

   type Scenario_Result is
     array (Positive range <>) of Scenario_Result_Element;

   procedure Reset
     (res           : in out Scenario_Result_Element'Class;
      reference_res : Scenario_Result_Element'Class);

   procedure Set_Prices
     (res      : in out Scenario_Result_Element'Class;
      last_res : Scenario_Result_Element'Class);

   procedure Kernel
     (curr    : Core.Kernel_Element;
      prev    : Core.Kernel_Element;
      conf    : Core.Scenario_Config;
      index   : Positive;
      results : in out Core.Scenario_Result);

   function Make_HA_Candle
     (current_candle : Candle_Base'Class; previous_candle : Candle_Base'Class)
      return HA_Candle;

   function Make_HA_Candles (in_candles : Candles) return HA_Candles;

end Core;
