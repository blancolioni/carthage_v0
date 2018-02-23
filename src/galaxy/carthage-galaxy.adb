package body Carthage.Galaxy is

   ----------------
   -- Jump_Route --
   ----------------

   function Jump_Route
     (Planet_1, Planet_2 : Carthage.Planets.Planet_Type)
      return Carthage.Planets.Array_Of_Planets
   is
      Vertices : constant Planet_Graph.Array_Of_Vertices :=
                   Graph.Path_Vertices
                     (Graph.Shortest_Path
                        (Index_Of (Planet_1), Index_Of (Planet_2)));
   begin
      return Ps : Carthage.Planets.Array_Of_Planets (Vertices'Range) do
         for I in Ps'Range loop
            Ps (I) := Graph.Vertex (Vertices (I));
         end loop;
      end return;
   end Jump_Route;

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
