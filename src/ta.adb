with Ada.Text_IO;

package body TA is

   function dummy return Boolean is
   begin
      return True;
   end dummy;

   procedure Calc_TA_ATR
     (startIdx        : Integer;
      endIdx          : Integer;
      inHigh          : Real_Array;
      inLow           : Real_Array;
      inClose         : Real_Array;
      optInTimePeriod : Integer;
      outBegIdx       : out Integer;
      outNBElement    : out Integer;
      outReal         : out Real_Array)
   is
      result : Integer;
   begin
      result :=
        TA_ATR
          (startIdx        => Interfaces.C.int (startIdx),
           endIdx          => Interfaces.C.int (endIdx),
           inHigh          => inHigh'Address,
           inLow           => inLow'Address,
           inClose         => inClose'Address,
           optInTimePeriod => Interfaces.C.int (optInTimePeriod),
           outBegIdx       => outBegIdx'Address,
           outNBElement    => outNBElement'Address,
           outReal         => outReal'Address);
   end Calc_TA_ATR;
end TA;
