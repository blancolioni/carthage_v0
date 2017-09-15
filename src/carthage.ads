package Carthage is

   Planet_Width  : constant := 44;
   Planet_Height : constant := 32;

   type Tile_X is range 1 .. Planet_Width;
   type Tile_Y is range 1 .. Planet_Height;

   type Tile_Position is
      record
         X : Tile_X;
         Y : Tile_Y;
      end record;

   type Array_Of_Positions is array (Positive range <>) of Tile_Position;

   function Tile_Position_Index
     (Position : Tile_Position)
      return Positive
   is (Natural (Position.Y - 1) * Planet_Width + Positive (Position.X));

end Carthage;
