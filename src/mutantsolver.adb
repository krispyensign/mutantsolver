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
   Oanda_Root    : TOML.TOML_Value;

   type Oanda_Access is record
      Token      : Unbounded.Unbounded_String;
      Account_ID : Unbounded.Unbounded_String;
      URL        : Unbounded.Unbounded_String;
   end record;

   Oanda : Oanda_Access;

   function Load_Oanda return Oanda_Access;

   function Load_Oanda return Oanda_Access is
   begin
      Oanda_Root := Result.Value.Get ("oanda");
      return
        (Token      =>
           Unbounded.To_Unbounded_String (Oanda_Root.Get ("token").As_String),
         Account_ID =>
           Unbounded.To_Unbounded_String
             (Oanda_Root.Get ("account_id").As_String),
         URL        =>
           Unbounded.To_Unbounded_String (Oanda_Root.Get ("url").As_String));
   end Load_Oanda;

begin
   if Result.Success then
      Text_IO.Put_Line ("config loaded with success!");
      Text_IO.Put_Line (Result.Value.Dump_As_String);
   else
      Text_IO.Put_Line ("error while loading config:");
      Text_IO.Put_Line (Unbounded.To_String (Result.Message));
      raise Program_Error;
   end if;

   Oanda := Load_Oanda;
   Local_Headers.Add ("Bearer", Unbounded.To_String (Oanda.Token));
   --  Data := Client.Get (URL => Oanda.URL & "/v3/instruments");
   --  Text_IO.Put (Response.Message_Body (Data));
   Log.Info ("Hello World!");
end Mutantsolver;
