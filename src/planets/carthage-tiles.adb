with Ada.Strings.Fixed;

with Carthage.Cities;

package body Carthage.Tiles is

   --------------------
   -- Position_Image --
   --------------------

   function Position_Image (Position : Tile_Position) return String
   is ("("
       & Ada.Strings.Fixed.Trim (Tile_X'Image (Position.X), Ada.Strings.Left)
       & ","
       & Ada.Strings.Fixed.Trim (Tile_Y'Image (Position.Y), Ada.Strings.Left)
       & ")");

   ---------------
   -- Add_Stack --
   ---------------

   procedure Add_Stack
     (Tile  : in out Tile_Record;
      Stack : not null access constant Carthage.Stacks.Stack_Record'Class)
   is
   begin
      Tile.Stacks.Append (Stack);
   end Add_Stack;

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

   ----------------
   -- Find_Stack --
   ----------------

   function Find_Stack
     (Tile  : Tile_Record;
      Match : not null access
        function (Stack : not null access constant
                    Carthage.Stacks.Stack_Record'Class)
      return Boolean)
      return access constant Carthage.Stacks.Stack_Record'Class
   is
   begin
      for Stack of Tile.Stacks loop
         if Match (Stack) then
            return Stack;
         end if;
      end loop;
      return null;
   end Find_Stack;

   ------------------
   -- Remove_Stack --
   ------------------

   procedure Remove_Stack
     (Tile  : in out Tile_Record;
      Stack : not null access constant Carthage.Stacks.Stack_Record'Class)
   is
      S : constant Stack_Access := Stack_Access (Stack);
      Position : Stack_Lists.Cursor := Tile.Stacks.First;
   begin
      while Stack_Lists.Element (Position) /= S loop
         Stack_Lists.Next (Position);
      end loop;
      Tile.Stacks.Delete (Position);
   end Remove_Stack;

   -------------------
   -- Remove_Stacks --
   -------------------

   procedure Remove_Stacks
     (Tile  : in out Tile_Record;
      Match : not null access
        function (Stack : not null access constant
                    Carthage.Stacks.Stack_Record'Class)
      return Boolean)
   is
      Position : Stack_Lists.Cursor := Tile.Stacks.First;
   begin
      while Stack_Lists.Has_Element (Position) loop
         declare
            Current : Stack_Lists.Cursor := Position;
         begin
            Stack_Lists.Next (Position);
            if Match (Stack_Lists.Element (Current)) then
               Tile.Stacks.Delete (Current);
            end if;
         end;
      end loop;
   end Remove_Stacks;

   -----------------
   -- Scan_Stacks --
   -----------------

   procedure Scan_Stacks
     (Tile    : Tile_Record;
      Process : not null access
        procedure (Stack : not null access constant
                     Carthage.Stacks.Stack_Record'Class))
   is
   begin
      for Stack of Tile.Stacks loop
         Process (Stack);
      end loop;
   end Scan_Stacks;

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
