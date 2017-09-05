with Ada.Strings.Fixed;

with Carthage.Cities;

package body Carthage.Tiles is

   function Position_Image (Position : Tile_Position) return String
   is ("("
       & Ada.Strings.Fixed.Trim (Tile_X'Image (Position.X), Ada.Strings.Left)
       & ","
       & Ada.Strings.Fixed.Trim (Tile_Y'Image (Position.Y), Ada.Strings.Left)
       & ")");

   -----------------
   -- Clear_Stack --
   -----------------

   procedure Clear_Stack
     (Tile : in out Tile_Record)
   is
   begin
      Tile.Stack := null;
   end Clear_Stack;

   ----------------------
   -- Clear_Visibility --
   ----------------------

   procedure Clear_Visibility
     (Tile : in out Tile_Record)
   is
   begin
      Carthage.Houses.Clear (Tile.Visible);
   end Clear_Visibility;

   -----------------
   -- Description --
   -----------------

   function Description
     (Tile : Tile_Record)
      return String
   is
      use type Carthage.Terrain.Terrain_Type;
      Terrain : constant String :=
        (if Tile.Terrain (2) /= null
         and then Tile.Terrain (2) /= Tile.Terrain (1)
         then Tile.Terrain (1).Identifier
         & " " & Tile.Terrain (2).Identifier
         else Tile.Terrain (1).Identifier);
      City : constant String :=
               (if Tile.Has_City
                then Tile.City.Structure.Identifier else "");
   begin
      return Position_Image (Tile.Position)
        & " "
        & (if City = "" then "" else City & " ") & Terrain;
   end Description;

   --------------
   -- Set_City --
   --------------

   procedure Set_City
     (Tile : in out Tile_Record;
      City : not null access constant Carthage.Cities.City_Record'Class)
   is
   begin
      Tile.City := City;
   end Set_City;

   ------------------------------
   -- Set_Currently_Visible_To --
   ------------------------------

   procedure Set_Currently_Visible_To
     (Tile  : in out Tile_Record;
      House : Carthage.Houses.House_Type)
   is
   begin
      Carthage.Houses.Insert (Tile.Visible, House);
      Carthage.Houses.Insert (Tile.Explored, House);
      Carthage.Houses.Insert (Tile.Seen, House);
   end Set_Currently_Visible_To;

   ---------------------
   -- Set_Explored_By --
   ---------------------

   procedure Set_Explored_By
     (Tile  : in out Tile_Record;
      House : Carthage.Houses.House_Type)
   is
   begin
      Carthage.Houses.Insert (Tile.Explored, House);
      Carthage.Houses.Insert (Tile.Seen, House);
   end Set_Explored_By;

   --------------
   -- Set_Road --
   --------------

   procedure Set_Road
     (Tile : in out Tile_Record;
      Road : Boolean)
   is
   begin
      Tile.Road := Road;
   end Set_Road;

   -----------------
   -- Set_Seen_By --
   -----------------

   procedure Set_Seen_By
     (Tile  : in out Tile_Record;
      House : Carthage.Houses.House_Type)
   is
   begin
      Carthage.Houses.Insert (Tile.Seen, House);
   end Set_Seen_By;

   ---------------
   -- Set_Stack --
   ---------------

   procedure Set_Stack
     (Tile  : in out Tile_Record;
      Stack : not null access constant Carthage.Stacks.Stack_Record'Class)
   is
   begin
      Tile.Stack := Stack;
   end Set_Stack;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Tile_Record'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Carthage.Tiles;
