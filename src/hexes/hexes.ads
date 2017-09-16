package Hexes is

   type Coordinate_Type is new Integer;

   subtype Distance_Type is Coordinate_Type range 0 .. Coordinate_Type'Last;

   type Cube_Coordinate is private;

   function "+" (Left, Right : Cube_Coordinate) return Cube_Coordinate;
   function "-" (Left, Right : Cube_Coordinate) return Cube_Coordinate;

   type Cube_Coordinate_Array is array (Positive range <>) of Cube_Coordinate;

   function Distance (From, To : Cube_Coordinate) return Distance_Type;

   function Neighbours
     (Hex : Cube_Coordinate)
      return Cube_Coordinate_Array;

   function Coordinates_Within
     (Hex      : Cube_Coordinate;
      Distance : Distance_Type)
      return Cube_Coordinate_Array;

   function Image (Hex : Cube_Coordinate) return String;

   type Axial_Coordinate is private;

   type Axial_Coordinate_Array is
     array (Positive range <>) of Axial_Coordinate;

   function Neighbours
     (Hex : Axial_Coordinate)
      return Axial_Coordinate_Array;

   function Coordinates_Within
     (Hex      : Axial_Coordinate;
      Distance : Distance_Type)
      return Axial_Coordinate_Array;

   function Image (Hex : Axial_Coordinate) return String;

   function To_Axial
     (Cube : Cube_Coordinate)
      return Axial_Coordinate;

   function To_Cube
     (Axial : Axial_Coordinate)
      return Cube_Coordinate;

private

   type Cube_Coordinate is
      record
         X, Y, Z : Coordinate_Type := 0;
      end record
   with Invariant => X + Y + Z = 0;

   function "+" (Left, Right : Cube_Coordinate) return Cube_Coordinate
   is (Left.X + Right.X, Left.Y + Right.Y, Left.Z + Right.Z);

   function "-" (Left, Right : Cube_Coordinate) return Cube_Coordinate
   is (Left.X - Right.X, Left.Y - Right.Y, Left.Z - Right.Z);

   type Axial_Coordinate is
      record
         Q, R : Coordinate_Type := 0;
      end record;

   function To_Axial
     (Cube : Cube_Coordinate)
      return Axial_Coordinate
   is (Cube.X, Cube.Y);

   function To_Cube
     (Axial : Axial_Coordinate)
      return Cube_Coordinate
   is (Axial.Q, Axial.R, -Axial.Q - Axial.R);

   function To_Axial_Array
     (Cubes : Cube_Coordinate_Array)
      return Axial_Coordinate_Array;

   function To_Cube_Array
     (Axials : Axial_Coordinate_Array)
      return Cube_Coordinate_Array;

   function Image (Hex : Cube_Coordinate) return String
   is ("<x" & Hex.X'Img & " y" & Hex.Y'Img & " z" & Hex.Z'Img & ">");

   function Image (Hex : Axial_Coordinate) return String
   is ("<q" & Hex.Q'Img & " r" & Hex.R'Img & ">");

end Hexes;
