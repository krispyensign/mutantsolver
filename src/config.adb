with Ada.Text_IO;
with Ada.Strings;

package body Config is

   function Map_TP_SL_Behavior (in_var : String) return Common.TPSL_Behavior is
   begin
      if in_var = "Default" then
         return Common.TPSL_Default;
      elsif in_var = "Self_Managed" then
         return Common.TPSL_Self_Managed;
      elsif in_var = "Dynamic" then
         return Common.TPSL_Dynamic;
      else
         raise Constraint_Error;
      end if;
   end Map_TP_SL_Behavior;

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
        (TPSL_Behavior          =>
           Map_TP_SL_Behavior (Chart_Root.Get ("tp_sl_behavior").As_String),
         Time_Period_Interval   =>
           Integer (Chart_Root.Get ("time_period_interval").As_Integer),
         Instrument             => Chart_Root.Get ("instrument").As_String,
         Num_Digits             =>
           Integer (Chart_Root.Get ("digits").As_Integer),
         Granularity            => Chart_Root.Get ("granularity").As_String,
         Offline_Set_Size       =>
           Integer (Chart_Root.Get ("offline_set_size").As_Integer),
         Online_Set_Size        =>
           Integer (Chart_Root.Get ("online_set_size").As_Integer),
         TP_SL_Offline_Set_Size =>
           Integer (Chart_Root.Get ("tp_sl_offline_set_size").As_Integer));
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
