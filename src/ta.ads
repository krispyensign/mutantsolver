pragma Ada_2022;

with Interfaces.C;
with System;

package TA is
   package cint renames Interfaces.C;

   function TA_Initialize return Integer
   with Import => True, Convention => C, External_Name => "TA_Initialize";

   function TA_ATR
     (startIdx        : Interfaces.C.int;
      endIdx          : Interfaces.C.int;
      inHigh          : System.Address;
      inLow           : System.Address;
      inClose         : System.Address;
      optInTimePeriod : Interfaces.C.int;
      outBegIdx       : access Interfaces.C.int;
      outNBElement    : access Interfaces.C.int;
      outReal         : System.Address) return Integer
   with Import => True, Convention => C, External_Name => "TA_ATR";

   function dummy return Boolean;

end TA;
