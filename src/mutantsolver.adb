pragma Ada_2022;

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Config; use Config;
with TOML;
with TOML.File_IO;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
with Ada.Strings.Fixed;

procedure Mutantsolver is
   result          : constant TOML.Read_Result :=
     TOML.File_IO.Load_File ("local_config.toml");
   oanda           : constant Oanda_Access := Load_Oanda (result);
   chart           : constant Chart_Config := Load_Chart_Config (result);

   --  construct URL to retrieve candles
   count           : constant Integer :=
     (chart.Train_Set_Size + chart.Sample_Set_Size);
   constructed_url : constant String :=
     Ada.Strings.Unbounded.To_String (oanda.URL)
     & "/v3/instruments/"
     & chart.Instrument
     & "/candles?price=MAB&granularity="
     & chart.Granularity
     & "&count="
     & Ada.Strings.Fixed.Trim (Count'Image, Ada.Strings.Left);

begin
   --  setup provider
   Util.Http.Clients.Curl.Register;

   --  start block that should be a separate function
   declare
      http     : Util.Http.Clients.Client;
      response : Util.Http.Clients.Response;
   begin
      --  setup headers
      http.Add_Header ("Content-Type", "application/json");
      http.Add_Header
        ("Bearer", Ada.Strings.Unbounded.To_String (Oanda.Token));
      http.Get (constructed_url, response);

      --  print to screen for now what the URL should look like
      Ada.Text_IO.Put_Line (response.Get_Body);
      Ada.Text_IO.Put_Line (Constructed_URL);
   end;

end Mutantsolver;
