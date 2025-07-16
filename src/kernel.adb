pragma Ada_2022;
with Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Text_IO;
with Interfaces.C;

package body Kernel is
   package io renames Ada.Text_IO;

   procedure Reset
     (res           : in out Scenario_Result_Element'Class;
      reference_res : Scenario_Result_Element'Class) is
   begin
      res.Entry_Price := 0.0;
      res.Exit_Price := 0.0;
      res.Trigger := 0;
      res.Signal := 0;
      res.Take_Profit_Price := 0.0;
      res.Stop_Loss_Price := 0.0;
      res.Position := 0.0;
      res.Exit_Value := 0.0;
      res.Exit_Total := reference_res.Exit_Total;
      res.Running_Total := reference_res.Exit_Total;
   end Reset;

   procedure Set_Prices
     (res      : in out Scenario_Result_Element'Class;
      last_res : Scenario_Result_Element'Class) is
   begin
      res.Entry_Price := last_res.Entry_Price;
      res.Take_Profit_Price := last_res.Take_Profit_Price;
      res.Stop_Loss_Price := last_res.Stop_Loss_Price;
      res.Exit_Total := last_res.Exit_Total;
   end Set_Prices;

   procedure Pin_Prices
     (curr                   : Common.Keyed_Lane;
      take_profit_multiplier : Float;
      stop_loss_multiplier   : Float;
      num_digits             : Positive;
      res                    : in out Scenario_Result_Element) is
   begin
      --  pin the entry to the ask close at this tick
      res.Entry_Price := curr (Common.Ask_Close);

      --  calc the tp and sl prices based on the pinned ask_close price
      if take_profit_multiplier /= 0.0 then
         res.Take_Profit_Price :=
           curr (Common.Ask_Close)
           + curr (Common.ATR) * Long_Float (take_profit_multiplier);
      end if;

      if stop_loss_multiplier /= 0.0 then
         res.Stop_Loss_Price :=
           curr (Common.Ask_Close)
           - curr (Common.ATR) * Long_Float (stop_loss_multiplier);
      end if;

      --  if sl is greater than the bid at close the set just below the bid
      --  close price to prevent order rejection
      if res.Stop_Loss_Price > curr (Common.Bid_Close) then
         res.Stop_Loss_Price :=
           curr (Common.Bid_Close) - Long_Float (10.0**(-num_digits));
      end if;

   end Pin_Prices;

   function Calc_WMA_Signal
     (curr           : Common.Keyed_Lane;
      prev           : Common.Keyed_Lane;
      entry_key      : Common.Candle_Key;
      exit_key       : Common.Candle_Key;
      wma_source_key : Common.WMA_Source_Key;
      last_res       : Scenario_Result_Element) return Scenario_Result_Element
   is
      buy_signal       : constant Boolean :=
        curr (entry_key) > curr (wma_source_key);
      prior_buy_signal : constant Boolean :=
        prev (entry_key) > prev (wma_source_key);
      exit_signal      : constant Boolean :=
        curr (exit_key) > curr (wma_source_key);
      res              : Scenario_Result_Element;
   begin
      res.Signal :=
        (if (not prior_buy_signal and then buy_signal)
           or else (prior_buy_signal and then exit_signal)
         then 1
         else 0);
      res.Trigger := res.Signal - last_res.Signal;

      return res;
   end Calc_WMA_Signal;

   procedure Kernel
     (curr    : Common.Keyed_Lane;
      prev    : Common.Keyed_Lane;
      conf    : Scenario_Config;
      index   : Positive;
      results : in out Scenario_Result)
   is
      bid_exit_price : constant Long_Float :=
        (if conf.Is_Quasi
         then curr (Common.Bid_Open)
         else curr (Common.Bid_Close));

      res      : Scenario_Result_Element := results (index);
      last_res : Scenario_Result_Element := results (index - 1);

   begin
      --  calculate the wma signal
      res :=
        Calc_WMA_Signal
          (curr,
           prev,
           conf.Entry_Key,
           conf.Exit_Key,
           conf.WMA_Source_Key,
           last_res);

      --  prepare the result with the previous result totals
      res.Exit_Total := last_res.Exit_Total;
      res.Min_Exit_Total := last_res.Min_Exit_Total;
      res.Max_Exit_Total := last_res.Max_Exit_Total;
      res.Wins := last_res.Wins;
      res.Losses := last_res.Losses;

      --  trigger, signal, notes
      --   0, 0, nothing
      --   1, 1, trigger
      --   0, 1, sustain -- check for tp/sl and calc more prices
      --  -1, 0, exit strategy -- check for tp/sl and calc more prices
      if res.Trigger = -1 and then last_res.Trigger = 1 and then conf.Is_Quasi
      then
         --  if quasi and the previous candle is an open and this candle
         --  is a close then erase and bail
         res.Reset (last_res);
         last_res.Reset (last_res);
         results (index) := res;
         results (index - 1) := last_res;

         return;
      elsif res.Trigger = 0 and then res.Signal = 0 then
         --  nothing is happening currently so bail
         results (index) := res;

         return;
      elsif res.Trigger = 1 and then res.Signal = 1 then
         --  wma cross so pin the prices
         Pin_Prices
           (curr,
            conf.Take_Profit_Multiplier,
            conf.Stop_Loss_Multiplier,
            conf.Num_Digits,
            res);
         results (index) := res;

         return;
      elsif res.Trigger = -1 and then res.Signal = 0 then
         --  wma cross so set the exit prices
         res.Exit_Price := bid_exit_price;
      end if;

      if conf.Use_Pinned_TPSL
        and then not ((res.Trigger = -1 and then res.Signal = 0)
                  or else (res.Trigger = 0 and then res.Signal = 1))
      then
         raise Constraint_Error;
      end if;

      --  set the prices
      res.Set_Prices (last_res);

      if res.Entry_Price = 0.0 then
         raise Constraint_Error;
      end if;

      if conf.Use_Pinned_TPSL then
         --  check for stop loss
         --  check the take profit
         if conf.Stop_Loss_Multiplier /= 0.0
           and then res.Stop_Loss_Price > curr (Common.Bid_Low)
         then
            res.Signal := 0;
            res.Trigger := res.Signal - last_res.Signal;
            res.Exit_Price := res.Stop_Loss_Price;
            res.Exit_Value := res.Exit_Price - res.Entry_Price;
            if res.Exit_Price - res.Entry_Price > 0.0 then
               raise Constraint_Error;
            end if;

         elsif conf.Take_Profit_Multiplier /= 0.0
           and then res.Take_Profit_Price < curr (Common.Bid_High)
         then
            res.Signal := 0;
            res.Trigger := res.Signal - last_res.Signal;
            res.Exit_Price := res.Take_Profit_Price;
            res.Exit_Value := res.Exit_Price - res.Entry_Price;
            if res.Exit_Price - res.Entry_Price < 0.0 then
               raise Constraint_Error;
            end if;
         end if;
      --  else
      --     declare
      --        position : constant Long_Float := bid_exit_price - res.Entry_Price;
      --     begin
      --        if conf.Take_Profit_Multiplier /= 0.0
      --          and then res.Trigger /= 1
      --          and then position
      --                   > curr (Common.ATR)
      --                     * Long_Float (conf.Take_Profit_Multiplier)
      --        then
      --           res.Signal := 0;
      --           res.Trigger := res.Signal - last_res.Signal;
      --           res.Exit_Price := bid_exit_price;
      --        end if;

      --        if conf.Stop_Loss_Multiplier /= 0.0
      --          and then position
      --                   < -curr (Common.ATR)
      --                     * Long_Float (conf.Stop_Loss_Multiplier)
      --        then
      --           res.Signal := 0;
      --           if res.Trigger = 1 then
      --              res.Entry_Price := 0.0;
      --              res.Exit_Price := 0.0;
      --           else
      --              res.Exit_Price := bid_exit_price;
      --           end if;
      --           res.Trigger := res.Signal - last_res.Signal;
      --        end if;
      --     end;
      end if;

      --  update the running and exit totals
      if res.Trigger = -1 then
         res.Exit_Value := res.Exit_Price - res.Entry_Price;
         res.Exit_Total := res.Exit_Total + res.Exit_Value;
         res.Running_Total := res.Exit_Total;
      elsif res.Signal = 1 then
         res.Position := bid_exit_price - curr (Common.Ask_Close);
         res.Running_Total := res.Exit_Total + res.Position;
      end if;

      --  update max total
      if res.Exit_Total /= 0.0
        and then res.Exit_Total > last_res.Max_Exit_Total
      then
         res.Max_Exit_Total := res.Exit_Total;
      end if;

      --  update min total
      if res.Exit_Total /= 0.0
        and then res.Exit_Total < last_res.Min_Exit_Total
      then
         res.Min_Exit_Total := res.Exit_Total;
      end if;

      --  update wins and losses
      if res.Exit_Value > 0.0 then
         res.Wins := res.Wins + 1;
      elsif res.Exit_Value < 0.0 then
         res.Losses := res.Losses + 1;
      end if;

      if res.Trigger = -1 and then res.Exit_Price = 0.0 then
         raise Constraint_Error;
      end if;

      results (index) := res;

   end Kernel;

   function Start_Procssing_Kernel
     (p : Common.Row_Pool; conf : Scenario_Config) return Scenario_Report
   is
      cur_scen_res : Scenario_Result (1 .. p'Length);
      last_element : Scenario_Result_Element;
      sr           : Scenario_Report;
   begin
      for i in conf.Start_Index .. p'Length loop
         --  calculate iteration
         Kernel
           (curr    => p (i),
            prev    => p (i - 1),
            index   => i,
            conf    => conf,
            results => cur_scen_res);
      end loop;

      --  skip further processing if criteria is not met
      last_element := cur_scen_res (p'Length - 1);
      sr :=
        (Wins           => last_element.Wins,
         Losses         => last_element.Losses,
         Max_Exit_Total => last_element.Max_Exit_Total,
         Min_Exit_Total => last_element.Min_Exit_Total,
         Final_Total    => last_element.Exit_Total,
         Ratio          => 0.0,
         Config         => conf);
      --  io.Put_Line
      --     ("so :"  & sr.Config.WMA_Source_Key'Image
      --     & " se:" & sr.Config.Entry_Key'Image
      --     & " sx:" & sr.Config.Exit_Key'Image
      --     & " tp:" & sr.Config.Take_Profit_Multiplier'Image
      --     & " sl:" & sr.Config.Stop_Loss_Multiplier'Image
      --     & " et :" & last_element.Exit_Total'Image
      --     & " w :" & last_element.Wins'Image
      --     & " l :" & last_element.Losses'Image);

      return sr;
   end Start_Procssing_Kernel;

   task body Process_Kernel is
      best_scenario_report : Scenario_Report;
      last_scenario_report : Scenario_Report;
      total_found          : Natural := 0;
      total_reported       : Natural := 0;
      writers              : Natural := 0;

      procedure Update_Scenario_Internal (sr : Scenario_Report) is
         ratio : Float;
      begin
         last_scenario_report := sr;
         total_reported := total_reported + 1;

         if best_scenario_report.Final_Total = Long_Float'First then
            best_scenario_report := sr;
         end if;

         if sr.Final_Total <= 0.0
           or else sr.Max_Exit_Total <= abs (sr.Min_Exit_Total)
         then
            return;
         end if;

         total_found := total_found + 1;
         io.Put_Line (sr'Image);

         ratio :=
           (if sr.Wins + sr.Losses > 0
            then Float (sr.Wins) / Float (sr.Wins + sr.Losses)
            else 0.0);

         if ratio >= best_scenario_report.Ratio
           and then sr.Final_Total >= best_scenario_report.Final_Total
         then
            best_scenario_report := sr;
            best_scenario_report.Ratio := ratio;
         end if;

      end Update_Scenario_Internal;
   begin
      loop
         select
            accept Start (p : Common.Row_Pool; conf : Scenario_Config) do
               declare
                  sr : Scenario_Report;
               begin
                  sr := Start_Procssing_Kernel (p, conf);
                  writers := writers + 1;
                  Update_Scenario_Internal (sr);
                  writers := writers - 1;
               end;
            end Start;
         or
            when writers = 0 =>
            accept Update_Scenario (sr : Scenario_Report) do
               writers := writers + 1;
               Update_Scenario_Internal (sr);
               writers := writers - 1;
            end Update_Scenario;
         or
            when writers = 0 =>
            accept Read (sr : out Scenario_Report; tf : out Natural) do
               sr := best_scenario_report;
               tf := total_found;
               if sr.Final_Total = 0.0 then
                  sr := last_scenario_report;
               end if;
            end Read;
         or
            terminate;
         end select;
      end loop;
   end Process_Kernel;

end Kernel;
