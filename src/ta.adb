pragma Ada_2022;
with Ada.Text_IO;

package body TA is
   package io renames Ada.Text_IO;

   procedure Calc_TA_ATR
     (in_high     : Real_Array;
      in_low      : Real_Array;
      in_close    : Real_Array;
      time_period : Integer;
      out_real    : out Real_Array)
   is
      result  : Integer;
      out_idx : Integer;
      out_end : Integer;
      temp    : Real_Array (1 .. out_real'Length);
   begin
      result :=
        TA_ATR
          (startIdx        => Interfaces.C.int (0),
           endIdx          => Interfaces.C.int (in_high'Length - 1),
           inHigh          => in_high'Address,
           inLow           => in_low'Address,
           inClose         => in_close'Address,
           optInTimePeriod => Interfaces.C.int (time_period),
           outBegIdx       => out_idx'Address,
           outNBElement    => out_end'Address,
           outReal         => temp'Address);
      out_real (1 .. out_idx) := temp (out_end + 1 .. out_real'Length);
      out_real (out_idx + 1 .. out_real'Length) := temp (1 .. out_end);
   end Calc_TA_ATR;
end TA;
