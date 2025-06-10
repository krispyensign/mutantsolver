with Ada.Text_IO;

package body Config is

   function Load_Oanda (Result : TOML.Read_Result) return Oanda_Access is
      Oanda_Root : constant TOML.TOML_Value := Result.Value.Get ("oanda");
   begin
      return
        (Token      => Oanda_Root.Get ("token").As_Unbounded_String,
         Account_ID => Oanda_Root.Get ("account_id").As_Unbounded_String,
         URL        => Oanda_Root.Get ("url").As_Unbounded_String);
   end Load_Oanda;

   function Load_Chart_Config (Result : TOML.Read_Result) return Chart_Config
   is
      Chart_Root : constant TOML.TOML_Value := Result.Value.Get ("chart");
   begin
      return
        (Instrument  => Chart_Root.Get ("instrument").As_String,
         Num_Digits  => Integer (Chart_Root.Get ("digits").As_Integer),
         Granularity => Chart_Root.Get ("granularity").As_String);
   end Load_Chart_Config;

   procedure Check_Load_Config_Result (Result : TOML.Read_Result) is
   begin
      if Result.Success then
         Text_IO.Put_Line ("config loaded with success!");
         Text_IO.Put_Line (Result.Value.Dump_As_String);
      else
         Text_IO.Put_Line ("error while loading config:");
         Text_IO.Put_Line (Unbounded.To_String (Result.Message));
         raise Program_Error;
      end if;
   end Check_Load_Config_Result;

end Config;
