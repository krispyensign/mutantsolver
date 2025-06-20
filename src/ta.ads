pragma Ada_2022;

with Interfaces.C;
with System;

package TA is

   type Real_Array is array (Positive range <>) of Long_Float;

   function TA_Initialize return Integer
   with Import => True, Convention => C, External_Name => "TA_Initialize";

   procedure Calc_TA_ATR
     (in_high     : Real_Array;
      in_low      : Real_Array;
      in_close    : Real_Array;
      time_period : Integer;
      out_real    : out Real_Array)
   with
     Pre =>
       in_high'Length = in_low'Length
       and then in_high'Length = in_close'Length
       and then in_high'Length = out_real'Length
       and then time_period < in_high'Length
       and then time_period > 0;

   procedure Calc_TA_WMA
     (in_real : Real_Array; time_period : Integer; out_real : out Real_Array)
   with
     Pre =>
       in_real'Length = out_real'Length
       and then time_period < in_real'Length
       and then time_period > 0;

private
   function TA_ATR
     (startIdx        : Interfaces.C.int;
      endIdx          : Interfaces.C.int;
      inHigh          : System.Address;
      inLow           : System.Address;
      inClose         : System.Address;
      optInTimePeriod : Interfaces.C.int;
      outBegIdx       : System.Address;
      outNBElement    : System.Address;
      outReal         : System.Address) return Integer
   with Import => True, Convention => C, External_Name => "TA_ATR";

   function TA_WMA
     (startIdx        : Interfaces.C.int;
      endIdx          : Interfaces.C.int;
      inReal          : System.Address;
      optInTimePeriod : Interfaces.C.int;
      outBegIdx       : System.Address;
      outNBElement    : System.Address;
      outReal         : System.Address) return Integer
   with Import => True, Convention => C, External_Name => "TA_WMA";

end TA;
