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
      Instrument             : String (1 .. 7);
      Num_Digits             : Integer;
      Granularity            : String (1 .. 2);
      Offline_Set_Size       : Integer;
      Online_Set_Size        : Integer;
      TP_SL_Offline_Set_Size : Integer;
      Time_Period_Interval   : Integer;
   end record;

   function Load_Oanda (Result : TOML.Read_Result) return Oanda_Access;

   function Load_Chart_Config (Result : TOML.Read_Result) return Chart_Config
   with 
      Post => Load_Chart_Config'Result.Instrument'Length > 0
        and then Load_Chart_Config'Result.Granularity'Length > 0
        and then Load_Chart_Config'Result.Time_Period_Interval > 0
        and then Load_Chart_Config'Result.Offline_Set_Size > Load_Chart_Config'Result.Time_Period_Interval
        and then Load_Chart_Config'Result.Online_Set_Size > Load_Chart_Config'Result.Time_Period_Interval
        and then Load_Chart_Config'Result.TP_SL_Offline_Set_Size > Load_Chart_Config'Result.Time_Period_Interval
        and then Load_Chart_Config'Result.TP_SL_Offline_Set_Size < Load_Chart_Config'Result.Offline_Set_Size;

   procedure Check_Load_Config_Result (Result : TOML.Read_Result);
end Config;
