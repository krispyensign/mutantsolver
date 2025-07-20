pragma Ada_2022;
with Core;
with Common;

package Kernel is

   type Kernel_Element is tagged record
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

   procedure Reset
     (res : in out Kernel_Element'Class; reference_res : Kernel_Element'Class);

   procedure Pin_Prices
     (res                    : in out Kernel_Element'Class;
      ask_close : Long_Float;
      bid_close : Long_Float;
      atr : Long_Float;
      take_profit_multiplier : Float;
      stop_loss_multiplier   : Float;
      num_digits             : Positive);

   procedure Carry_Over_Prices
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class);

   procedure Carry_Over_Totals
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class);

   procedure Execute_Stop_Loss (res : in out Kernel_Element'Class);
   procedure Execute_Take_Profit (res : in out Kernel_Element'Class);
   procedure Update_Min_Max_Totals
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class);
   procedure Update_Wins_Losses (res : in out Kernel_Element'Class);
   procedure Update_Exit_Totals (res : in out Kernel_Element'Class);
   procedure Update_Position
     (res            : in out Kernel_Element'Class;
      bid_exit_price : Long_Float;
      ask_close      : Long_Float);

   type Kernel_Elements is array (Positive range <>) of Kernel_Element;

   type Scenario_Config is tagged record
      Start_Index            : Positive;
      Is_Quasi               : Boolean := False;
      Should_Roll            : Boolean := False;
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

   procedure Kernel
     (curr      : Common.Keyed_Lane;
      prev      : Common.Keyed_Lane;
      prev_prev : Common.Keyed_Lane;
      conf      : Scenario_Config;
      index     : Positive;
      results   : in out Kernel_Elements);

end Kernel;
