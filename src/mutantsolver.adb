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

   type Oanda_Access is record
      Token      : String (1 .. 80);
      Account_ID : String (1 .. 80);
      URL        : String (1 .. 256);
   end record;

   Oanda : Oanda_Access;

begin
   if Result.Success then
      Text_IO.Put_Line ("config.toml loaded with success!");
   else
      Text_IO.Put_Line ("error while loading config.toml:");
      Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Result.Message));
   end if;

   Oanda :=
     (Token      => Result.Value.Get ("oanda").Get ("token").As_String,
      Account_ID => Result.Value.Get ("oanda").Get ("account_id").As_String,
      URL        => Result.Value.Get ("oanda").Get ("url").As_String);
   Local_Headers.Add ("Bearer", Oanda.Token);
   Data := Client.Get (URL => Oanda.URL & "/v3/instruments");
   Text_IO.Put (Response.Message_Body (Data));
   Log.Info ("Hello World!");
end Mutantsolver;
