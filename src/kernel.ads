pragma Ada_2022;
with Core;
with Common;
use type Common.TPSL_Behavior;

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
      Exit_Behavior          : Common.TPSL_Behavior;
   end record;

   procedure Kernel
     (curr      : Common.Keyed_Lane;
      prev      : Common.Keyed_Lane;
      prev_prev : Common.Keyed_Lane;
      conf      : Scenario_Config;
      index     : Positive;
      results   : in out Kernel_Elements);

private

   --  reset the Kernel_Element to a reference state
   procedure Reset
     (res : in out Kernel_Element'Class; reference_res : Kernel_Element'Class);

   --  pin the prices to the current candle
   procedure Pin_TPSL_Prices
     (res                    : in out Kernel_Element'Class;
      last_res               : Kernel_Element'Class;
      ask_close              : Long_Float;
      bid_close              : Long_Float;
      atr                    : Long_Float;
      take_profit_multiplier : Float;
      stop_loss_multiplier   : Float;
      num_digits             : Positive;
      behavior               : Common.TPSL_Behavior);

   --  carry over prices from the last Kernel_Element
   procedure Carry_Over_Prices
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class);

   --  carry over totals from the last Kernel_Element
   procedure Carry_Over_Totals
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class);

   procedure Process_Self_Managed_Exits
      (res : in out Kernel_Element'Class;
       curr : Common.Keyed_Lane;
       conf : Scenario_Config);

   procedure Process_Broker_Managed_Exits
      (res : in out Kernel_Element'Class;
       last_res : Kernel_Element'Class;
       prev : Common.Keyed_Lane;
       curr : Common.Keyed_Lane;
       conf : Scenario_Config'Class);

   --  update the min and max exit totals
   procedure Update_Min_Max_Totals
     (res : in out Kernel_Element'Class; last_res : Kernel_Element'Class);

   --  update the wins and losses
   procedure Update_Wins_Losses (res : in out Kernel_Element'Class);

   --  update the exit totals
   procedure Update_Exit_Totals (res : in out Kernel_Element'Class);

   --  update the position based on the exit price
   procedure Update_Position
     (res            : in out Kernel_Element'Class;
      bid_exit_price : Long_Float;
      ask_close      : Long_Float);

end Kernel;
