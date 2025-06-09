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
use GNATCOLL;

procedure Mutantsolver is
   package Log renames Simple_Logging;
   Result        : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   Data          : Response.Data;
   Local_Headers : Headers.List;
begin
   if Result.Success then
      Text_IO.Put_Line ("config.toml loaded with success!");
   else
      Text_IO.Put_Line ("error while loading config.toml:");
      Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Result.Message));
   end if;

   declare
      Oanda_Token      : constant String :=
        Result.Value.Get ("oanda").Get ("token").As_String;
      Oanda_Account_ID : constant String :=
        Result.Value.Get ("oanda").Get ("account_id").As_String;
      Oanda_URL        : constant String :=
        Result.Value.Get ("oanda").Get ("url").As_String;
   begin
      Local_Headers.Add ("Bearer", Oanda_Token);
      Data := Client.Get (URL => Oanda_URL & "/v3/instruments");
      Text_IO.Put (Response.Message_Body (Data));
      Log.Info ("Hello World!");
   end;
end Mutantsolver;
