with Carthage.Tiles;

package body Carthage.Managers.Planets is

   package List_Of_Tiles is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Tiles.Tile_Type, Carthage.Tiles."=");

   type Planet_Manager_Record is
     new Manager_Record with
      record
         Planet             : Carthage.Planets.Planet_Type;
         Controlled_Tiles   : List_Of_Tiles.List;
         Uncontrolled_Tiles : List_Of_Tiles.List;
         Unseen_Tiles       : List_Of_Tiles.List;
      end record;

   overriding procedure Create_Initial_State
     (Manager : in out Planet_Manager_Record);

   overriding procedure Load_State
     (Manager : in out Planet_Manager_Record);

   --------------------------
   -- Create_Initial_State --
   --------------------------

   overriding procedure Create_Initial_State
     (Manager : in out Planet_Manager_Record)
   is
   begin
      null;
   end Create_Initial_State;

   ---------------------------
   -- Create_Planet_Manager --
   ---------------------------

   function Create_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Manager_Type
   is
      Manager : Planet_Manager_Record;
   begin
      Manager.Create (House);
      Manager.Planet := Planet;
      return new Planet_Manager_Record'(Manager);
   end Create_Planet_Manager;

   ----------------
   -- Load_State --
   ----------------

   overriding procedure Load_State
     (Manager : in out Planet_Manager_Record)
   is
   begin
      Manager.Controlled_Tiles.Clear;
      Manager.Uncontrolled_Tiles.Clear;
      Manager.Unseen_Tiles.Clear;

      for Y in Tile_Y loop
         for X in Tile_X loop
            declare
               Tile : constant Carthage.Tiles.Tile_Type :=
                        Manager.Planet.Tile (X, Y);
            begin
               if Tile.Currently_Visible_To (Manager.House) then
                  Manager.Controlled_Tiles.Append (Tile);
               elsif Tile.Seen_By (Manager.House) then
                  Manager.Uncontrolled_Tiles.Append (Tile);
               else
                  Manager.Unseen_Tiles.Append (Tile);
               end if;
            end;
         end loop;
      end loop;
   end Load_State;

end Carthage.Managers.Planets;
