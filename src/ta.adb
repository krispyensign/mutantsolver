pragma Ada_2022;

package body TA is

   function Calc_TA_ATR
     (in_high     : Common.Real_Array;
      in_low      : Common.Real_Array;
      in_close    : Common.Real_Array;
      time_period : Integer) return Common.Real_Array
   is
      result   : Integer;
      out_idx  : Integer;
      out_end  : Integer;
      temp     : Common.Real_Array (1 .. in_high'Length);
      out_real : Common.Real_Array (1 .. in_high'Length);
   begin
      if not is_initialized then
         result := TA_Initialize;
         if result /= 0 then
            raise Program_Error;
         end if;
         is_initialized := True;
      end if;
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
      return out_real;
   end Calc_TA_ATR;

   function Calc_TA_WMA
     (in_real : Common.Real_Array; time_period : Integer)
      return Common.Real_Array
   is
      result   : Integer;
      out_idx  : Integer;
      out_end  : Integer;
      temp     : Common.Real_Array (1 .. in_real'Length);
      out_real : Common.Real_Array (1 .. in_real'Length);
   begin
      if not is_initialized then
         result := TA_Initialize;
         if result /= 0 then
            raise Program_Error;
         end if;
         is_initialized := True;
      end if;
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
      return out_real;
   end Calc_TA_WMA;

end TA;
