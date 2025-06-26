pragma Ada_2022;

package body TA is

   procedure Calc_TA_ATR
     (in_high     : Core.Real_Array;
      in_low      : Core.Real_Array;
      in_close    : Core.Real_Array;
      time_period : Integer;
      out_real    : Core.Real_Array_Ptr)
   is
      result  : Integer;
      out_idx : Integer;
      out_end : Integer;
      temp    : Core.Real_Array (1 .. out_real'Length);
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
      if result /= 0 then
         raise Program_Error;
      end if;
      out_real (1 .. out_idx) := temp (out_end + 1 .. out_real'Length);
      out_real (out_idx + 1 .. out_real'Length) := temp (1 .. out_end);
   end Calc_TA_ATR;

   procedure Calc_TA_WMA
     (in_real     : Core.Real_Array;
      time_period : Integer;
      out_real    : Core.Real_Array_Ptr)
   is
      result  : Integer;
      out_idx : Integer;
      out_end : Integer;
      temp    : Core.Real_Array (1 .. out_real'Length);
   begin
      result :=
        TA_WMA
          (startIdx        => Interfaces.C.int (0),
           endIdx          => Interfaces.C.int (in_real'Length - 1),
           inReal          => in_real'Address,
           optInTimePeriod => Interfaces.C.int (time_period),
           outBegIdx       => out_idx'Address,
           outNBElement    => out_end'Address,
           outReal         => temp'Address);
      if result /= 0 then
         raise Program_Error;
      end if;
      out_real (1 .. out_idx) := temp (out_end + 1 .. out_real'Length);
      out_real (out_idx + 1 .. out_real'Length) := temp (1 .. out_end);
   end Calc_TA_WMA;

end TA;
