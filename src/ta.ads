pragma Ada_2022;

with Interfaces.C;
with System;

package TA is

   function TA_Initialize return Integer
   with Import => True, Convention => C, External_Name => "TA_Initialize";

   type Real_Array is array (Positive range <>) of Long_Float;
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
       and then time_period < in_high'Length
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



end TA;
