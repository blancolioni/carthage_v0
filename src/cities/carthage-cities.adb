package body Carthage.Cities is

   ------------------------
   -- Scan_Planet_Cities --
   ------------------------

   procedure Scan_Planet_Cities
     (Planet  : Carthage.Planets.Planet_Type;
      Process : not null access
        procedure (City : City_Type))
   is
      use Carthage.Planets;

      function Same_Planet (City : City_Type) return Boolean
      is (City.Planet = Planet);

   begin
      Db.Scan (Same_Planet'Access, Process);
   end Scan_Planet_Cities;

end Carthage.Cities;
