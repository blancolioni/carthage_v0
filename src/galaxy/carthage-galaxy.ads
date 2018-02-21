private with WL.Graphs;

with Carthage.Planets;

package Carthage.Galaxy is

   procedure Scan_Connections
     (From : Carthage.Planets.Planet_Type;
      Process : not null access
        procedure (Planet : Carthage.Planets.Planet_Type));

   function Connected
     (Planet_1, Planet_2 : Carthage.Planets.Planet_Type)
      return Boolean;

private

   function Index_Of (Planet : Carthage.Planets.Planet_Type) return Positive
   is (Planet.Index);

   package Planet_Graph is
     new WL.Graphs
       (Index_Type   => Positive,
        Vertex_Type  => Carthage.Planets.Planet_Type,
        Cost_Type    => Float,
        Default_Cost => 1.0,
        Index_Of     => Index_Of,
        "="          => Carthage.Planets."=");

   Graph : Planet_Graph.Graph;

   function Connected
     (Planet_1, Planet_2 : Carthage.Planets.Planet_Type)
      return Boolean
   is (Graph.Connected (Index_Of (Planet_1), Index_Of (Planet_2)));

end Carthage.Galaxy;
