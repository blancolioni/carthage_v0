package body Carthage.Galaxy is

   ----------------------
   -- Scan_Connections --
   ----------------------

   procedure Scan_Connections
     (From : Carthage.Planets.Planet_Type;
      Process : not null access
        procedure (Planet : Carthage.Planets.Planet_Type))
   is
      procedure Process_Edge
        (Planet : Carthage.Planets.Planet_Type;
         Cost   : Float);

      ------------------
      -- Process_Edge --
      ------------------

      procedure Process_Edge
        (Planet : Carthage.Planets.Planet_Type;
         Cost   : Float)
      is
         pragma Unreferenced (Cost);
      begin
         Process (Planet);
      end Process_Edge;

   begin
      Graph.Iterate_Edges
        (From, Process_Edge'Access);
   end Scan_Connections;

end Carthage.Galaxy;
