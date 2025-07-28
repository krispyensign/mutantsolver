pragma Ada_2022;

with TOML;
with Ada.Strings.Unbounded;
use Ada;
use Ada.Strings;
with Common;

package Config is
   type Oanda_Access is record
      Token      : Unbounded.Unbounded_String;
      Account_ID : Unbounded.Unbounded_String;
      URL        : Unbounded.Unbounded_String;
   end record;

   type Chart_Config is record
      Instrument             : String (1 .. 7);
      Num_Digits             : Positive;
      Granularity            : String (1 .. 2);
      Offline_Set_Size       : Positive;
      Online_Set_Size        : Positive;
      TP_SL_Offline_Set_Size : Positive;
      Time_Period_Interval   : Positive;
      TPSL_Behavior          : Common.TPSL_Behavior;
      Should_Screen_ATR      : Boolean;
   end record
   with
     Dynamic_Predicate =>
       Chart_Config.Instrument'Length = 7
       and then Chart_Config.Granularity'Length > 0
       and then Chart_Config.Offline_Set_Size
                > Chart_Config.Time_Period_Interval
       and then Chart_Config.Online_Set_Size
                > Chart_Config.Time_Period_Interval
       and then Chart_Config.TP_SL_Offline_Set_Size
                > Chart_Config.Time_Period_Interval
       and then Chart_Config.TP_SL_Offline_Set_Size
                < Chart_Config.Offline_Set_Size;

   function Load_Oanda (Result : TOML.Read_Result) return Oanda_Access;

   function Load_Chart_Config (Result : TOML.Read_Result) return Chart_Config;

   procedure Check_Load_Config_Result (Result : TOML.Read_Result);
end Config;
