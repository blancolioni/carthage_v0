package body Hexes is

   Cube_Directions : constant Cube_Coordinate_Array :=
                       (1 => (1, -1, 0),
                        2 => (1, 0, -1),
                        3 => (0, 1, -1),
                        4 => (-1, 1, 0),
                        5 => (-1, 0, 1),
                        6 => (0, -1, 1));

   ------------------------
   -- Coordinates_Within --
   ------------------------

   function Coordinates_Within
     (Hex      : Cube_Coordinate;
      Distance : Distance_Type)
      return Cube_Coordinate_Array
   is
      Vector : Cube_Vectors.Vector;
   begin
      for DX in -Distance .. Distance loop
         for DY in Coordinate_Type'Max (-Distance, -DX - Distance)
           .. Coordinate_Type'Min (Distance, -DX + Distance)
         loop
            Vector.Append (Hex + (DX, DY, -DX - DY));
         end loop;
      end loop;
      return To_Array (Vector);
   end Coordinates_Within;

   ------------------------
   -- Coordinates_Within --
   ------------------------

   function Coordinates_Within
     (Hex      : Axial_Coordinate;
      Distance : Distance_Type)
      return Axial_Coordinate_Array
   is
   begin
      return To_Axial_Array
        (Coordinates_Within (To_Cube (Hex), Distance));
   end Coordinates_Within;

   --------------
   -- Distance --
   --------------

   function Distance (From, To : Cube_Coordinate) return Distance_Type is
   begin
      return (abs (From.X - To.X) + abs (From.Y - To.Y) + abs (From.Z - To.Z))
        / 2;
   end Distance;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (Hex : Cube_Coordinate)
      return Cube_Coordinate_Array
   is
   begin
      return Ns : Cube_Coordinate_Array := Cube_Directions do
         for N of Ns loop
            N.X := N.X + Hex.X;
            N.Y := N.Y + Hex.Y;
            N.Z := N.Z + Hex.Z;
         end loop;
      end return;
   end Neighbours;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (Hex : Axial_Coordinate)
      return Axial_Coordinate_Array
   is
   begin
      return To_Axial_Array
        (Neighbours (To_Cube (Hex)));
   end Neighbours;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Vector : Cube_Vectors.Vector)
      return Cube_Coordinate_Array
   is
   begin
      return A : Cube_Coordinate_Array (1 .. Vector.Last_Index) do
         for I in A'Range loop
            A (I) := Vector (I);
         end loop;
      end return;
   end To_Array;

   --------------------
   -- To_Axial_Array --
   --------------------

   function To_Axial_Array
     (Cubes : Cube_Coordinate_Array)
      return Axial_Coordinate_Array
   is
   begin
      return Axials : Axial_Coordinate_Array (Cubes'Range) do
         for I in Axials'Range loop
            Axials (I) := To_Axial (Cubes (I));
         end loop;
      end return;
   end To_Axial_Array;

   -------------------
   -- To_Cube_Array --
   -------------------

   function To_Cube_Array
     (Axials : Axial_Coordinate_Array)
      return Cube_Coordinate_Array
   is
   begin
      return Cubes : Cube_Coordinate_Array (Axials'Range) do
         for I in Cubes'Range loop
            Cubes (I) := To_Cube (Axials (I));
         end loop;
      end return;
   end To_Cube_Array;

end Hexes;
