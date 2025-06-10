pragma Ada_2022;

with AWS.Client;
with AWS.Response;
with AWS.Headers;
with Ada.Text_IO;
with Config;
with GNATCOLL.JSON;
with Simple_Logging;
with TOML;
with TOML.File_IO;
with Ada.Strings.Unbounded;
use AWS;
use Ada;
use Ada.Strings;
use GNATCOLL;
use Config;

procedure Mutantsolver is
   package Log renames Simple_Logging;

   Result        : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   Data          : Response.Data;
   Local_Headers : Headers.List;

   Oanda : Oanda_Access;

   Chart : Chart_Config;

begin
   Check_Load_Config_Result (Result);
   Oanda := Load_Oanda (Result);
   Chart := Load_Chart_Config (Result);

   Local_Headers.Add ("Content-Type", "application/json");
   Local_Headers.Add ("Bearer", Unbounded.To_String (Oanda.Token));
   Data :=
     Client.Get
       (URL =>
          Unbounded.To_String (Oanda.URL)
          & "/v3/instruments/"
          & Chart.Instrument
          & "/candles?price=MAB&granularity="
          & Chart.Granularity);
   Text_IO.Put (Response.Message_Body (Data));
end Mutantsolver;
