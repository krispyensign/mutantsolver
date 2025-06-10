pragma Ada_2022;

with AWS.Client;
with AWS.Response;
with AWS.Headers;
with Ada.Text_IO;
with GNATCOLL.JSON;
with Simple_Logging;
with TOML;
with TOML.File_IO;
with Ada.Strings.Unbounded;
use AWS;
use Ada;
use Ada.Strings;
use GNATCOLL;

procedure Mutantsolver is
   package Log renames Simple_Logging;

   Result        : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   Data          : Response.Data;
   Local_Headers : Headers.List;

   type Oanda_Access is record
      Token      : Unbounded.Unbounded_String;
      Account_ID : Unbounded.Unbounded_String;
      URL        : Unbounded.Unbounded_String;
   end record;
   Oanda : Oanda_Access;

   type Chart_Config is record
      Instrument  : String (1 .. 7);
      Num_Digits  : Integer;
      Granularity : String (1 .. 2);
   end record;
   Chart : Chart_Config;

   function Load_Oanda return Oanda_Access;
   function Load_Oanda return Oanda_Access is
      Oanda_Root : constant TOML.TOML_Value := Result.Value.Get ("oanda");
   begin
      return
        (Token      => Oanda_Root.Get ("token").As_Unbounded_String,
         Account_ID => Oanda_Root.Get ("account_id").As_Unbounded_String,
         URL        => Oanda_Root.Get ("url").As_Unbounded_String);
   end Load_Oanda;

   function Load_Chart_Config return Chart_Config;
   function Load_Chart_Config return Chart_Config is
      Chart_Root : constant TOML.TOML_Value := Result.Value.Get ("chart");
   begin
      return
        (Instrument  => Chart_Root.Get ("instrument").As_String,
         Num_Digits  => Integer (Chart_Root.Get ("digits").As_Integer),
         Granularity => Chart_Root.Get ("granularity").As_String);
   end Load_Chart_Config;

   procedure Check_Load_Config_Result;
   procedure Check_Load_Config_Result is
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

begin
   Check_Load_Config_Result;

   Oanda := Load_Oanda;
   Chart := Load_Chart_Config;

   Local_Headers.Add ("Bearer", Unbounded.To_String (Oanda.Token));
   --  Data := Client.Get (URL => Oanda.URL & "/v3/instruments");
   --  Text_IO.Put (Response.Message_Body (Data));
   Log.Info ("Hello World!");
end Mutantsolver;
