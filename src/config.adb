with Ada.Strings;
with Util.Dates.ISO8601;

package body Config is
   package iso8601 renames Util.Dates.ISO8601;

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

   function Load_System (result : TOML.Read_Result) return System_Config is
      system_root : constant TOML.TOML_Value := result.Value.Get ("system");
   begin
      return (Cache_Dir => system_root.Get ("cache_dir").As_Unbounded_String);
   end Load_System;

   function Load_Oanda (result : TOML.Read_Result) return Oanda_Access is
      oanda_root : constant TOML.TOML_Value := result.Value.Get ("oanda");
   begin
      return
        (Token      => oanda_root.Get ("token").As_Unbounded_String,
         Account_ID => oanda_root.Get ("account_id").As_Unbounded_String,
         URL        => oanda_root.Get ("url").As_Unbounded_String);
   end Load_Oanda;

   function Load_Chart_Config (result : TOML.Read_Result) return Chart_Config
   is
      chart_root      : constant TOML.TOML_Value := result.Value.Get ("chart");
      has_dates       : constant Boolean := chart_root.Has ("dates");
      date_count      : constant Positive := chart_root.Get ("dates").Length;
      converted_dates : Date_List (1 .. date_count);
      toml_dates      : constant TOML.TOML_Value := chart_root.Get ("dates");
   begin
      if has_dates then
         for I in 1 .. date_count loop
            converted_dates (I) :=
              iso8601.Value (toml_dates.Item (I).As_String);
         end loop;
      end if;
      return
        (date_count             => date_count,
         Dates                  => converted_dates,
         TPSL_Behavior          =>
           Map_TP_SL_Behavior (chart_root.Get ("tp_sl_behavior").As_String),
         Should_Screen_ATR      =>
           chart_root.Get ("should_screen_atr").As_Boolean,
         Time_Period_Interval   =>
           Integer (chart_root.Get ("time_period_interval").As_Integer),
         Instrument             => chart_root.Get ("instrument").As_String,
         Num_Digits             =>
           Integer (chart_root.Get ("digits").As_Integer),
         Granularity            => chart_root.Get ("granularity").As_String,
         Offline_Set_Size       =>
           Integer (chart_root.Get ("offline_set_size").As_Integer),
         Online_Set_Size        =>
           Integer (chart_root.Get ("online_set_size").As_Integer),
         TP_SL_Offline_Set_Size =>
           Integer (chart_root.Get ("tp_sl_offline_set_size").As_Integer));
   end Load_Chart_Config;

end Config;
