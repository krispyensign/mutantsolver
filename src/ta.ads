pragma Ada_2022;

with Interfaces.C;
with System;
with Common;

package TA is
   function Calc_TA_ATR
     (in_high     : Common.Real_Array;
      in_low      : Common.Real_Array;
      in_close    : Common.Real_Array;
      time_period : Integer) return Common.Real_Array
   with
     Pre =>
       in_high'Length = in_low'Length
       and then in_high'Length = in_close'Length
       and then time_period < in_high'Length
       and then time_period > 0;

   function Calc_TA_WMA
     (in_real : Common.Real_Array; time_period : Integer)
      return Common.Real_Array
   with Pre => time_period < in_real'Length and then time_period > 0;

private
   is_initialized : Boolean := False;

   function TA_Initialize return Integer
   with Import => True, Convention => C, External_Name => "TA_Initialize";

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
