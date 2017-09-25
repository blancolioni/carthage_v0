package Carthage is

   Planet_Width  : constant := 44;
   Planet_Height : constant := 32;

   type Tile_X_Count is range 0 .. Planet_Width;
   type Tile_Y_Count is range 0 .. Planet_Height;

   subtype Tile_X is Tile_X_Count range 1 .. Planet_Width;
   subtype Tile_Y is Tile_Y_Count range 1 .. Planet_Height;

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

   type Resource_Quantity is delta 0.001 range 0.0 .. 999_999.0;

   type Health_Type is range 0 .. 100;
   type Loyalty_Type is range 0 .. 100;

end Carthage;
