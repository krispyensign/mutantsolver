pragma Ada_2022;
pragma Extensions_Allowed (On);
with Ada.Command_Line;

package body OpenMPI is
   package cmd renames Ada.Command_Line;

   function Find_Longest_Length (argc : Positive) return Positive is
      longest_len : Natural := 0;
   begin
      for i in 1 .. argc loop
         if cmd.Argument (i)'Length > longest_len then
            longest_len := cmd.Argument (i)'Length;
         end if;
      end loop;

      return longest_len;
   end Find_Longest_Length;

   procedure Init is
      argc : constant Positive := cmd.Argument_Count + 1;
      longest_len : constant Natural := Find_Longest_Length (argc);
      res : Integer := 0;
   begin
      argv : array (Positive range 1 .. argc + 1) of String (1 .. longest_len) :=
         [for i in 2 .. argc - 1 => cmd.Argument (i)];
      argv (1) := "mutantsolver";

      res := MPI_Init (argc, argv'Address);
      if res /= 0 then
         raise Program_Error;
      end if;
   end Init;

   procedure Finalize is
      res : Integer := 0;
   begin
      res := MPI_Finalize;
      if res /= 0 then
         raise Program_Error;
      end if;
   end Finalize;

   procedure Barrier is
   begin
      null;
   end Barrier;

end OpenMPI;
