with System;
package OpenMPI is
   procedure Init;
   procedure Finalize;
   procedure Barrier;

private

   MPI_COMM_WORLD : constant System.Address
      with Import => True, Convention => C, External_Name => "MPI_COMM_WORLD";

   function MPI_Init
      (argc : Integer; argv : System.Address) return Integer
       with Import => True, Convention => C, External_Name => "MPI_Init";

   function MPI_Barrier (comm : System.Address) return Integer
      with Import => True, Convention => C, External_Name => "MPI_Barrier";

   function MPI_Finalize return Integer
      with Import => True, Convention => C, External_Name => "MPI_Finalize";

end OpenMPI;
