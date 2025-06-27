pragma Ada_2022;

with TOML;
with Ada.Strings.Unbounded;
use Ada;
use Ada.Strings;

package Config is
   type Oanda_Access is record
      Token      : Unbounded.Unbounded_String;
      Account_ID : Unbounded.Unbounded_String;
      URL        : Unbounded.Unbounded_String;
   end record;

   type Chart_Config is record
      Instrument           : String (1 .. 7);
      Num_Digits           : Integer;
      Granularity          : String (1 .. 2);
      Train_Set_Size       : Integer;
      Sample_Set_Size      : Integer;
      TP_SL_Train_Set_Size : Integer;
      Time_Period_Interval : Integer;
   end record;

   function Load_Oanda (Result : TOML.Read_Result) return Oanda_Access;

   function Load_Chart_Config (Result : TOML.Read_Result) return Chart_Config;

   procedure Check_Load_Config_Result (Result : TOML.Read_Result);
end Config;
