pragma Ada_2022;

with Interfaces.C;
with System;

package TA is

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

   function dummy return Boolean;

   type Real_Array is array (Positive range <>) of Long_Float;
   procedure Calc_TA_ATR
     (startIdx        : Integer;
      endIdx          : Integer;
      inHigh          : Real_Array;
      inLow           : Real_Array;
      inClose         : Real_Array;
      optInTimePeriod : Integer;
      outBegIdx       : out Integer;
      outNBElement    : out Integer;
      outReal         : out Real_Array);

end TA;
