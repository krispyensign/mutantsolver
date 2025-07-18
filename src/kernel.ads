pragma Ada_2022;
with Core;
with Common;

package Kernel is

   type Scenario_Result_Element is tagged record
      Signal            : Core.Signal_T := 0;
      Trigger           : Core.Trigger_T := 0;
      Entry_Price       : Long_Float := 0.0;
      Exit_Price        : Long_Float := 0.0;
      Position          : Long_Float := 0.0;
      Take_Profit_Price : Long_Float := 0.0;
      Stop_Loss_Price   : Long_Float := 0.0;
      Running_Total     : Long_Float := 0.0;
      Exit_Value        : Long_Float := 0.0;

      Take_Profits   : Natural := 0;
      Stop_Losses    : Natural := 0;
      Exit_Total     : Long_Float := 0.0;
      Wins           : Natural := 0;
      Losses         : Natural := 0;
      Min_Exit_Total : Long_Float := Long_Float'Last;
      Max_Exit_Total : Long_Float := Long_Float'First;
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
      Take_Profits   : Natural := 0;
      Stop_Losses    : Natural := 0;
      Max_Exit_Total : Long_Float := Long_Float'First;
      Min_Exit_Total : Long_Float := Long_Float'Last;
      Ratio          : Float := 0.0;
      Final_Total    : Long_Float := Long_Float'First;
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
     (curr    : Common.Keyed_Lane;
      prev    : Common.Keyed_Lane;
      conf    : Scenario_Config;
      index   : Positive;
      results : in out Scenario_Result);

   task type Process_Kernel is
      entry Start (p : Common.Row_Pool; conf : Scenario_Config);
      entry Update_Scenario (sr : Scenario_Report);
      entry Read (sr : out Scenario_Report; tf : out Natural);
   end Process_Kernel;

end Kernel;
