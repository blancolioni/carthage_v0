with WL.Random;

with Carthage.Tiles;

with Carthage.Managers.Assets;

package body Carthage.Managers.Planets is

   package List_Of_Tiles is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Tiles.Tile_Type, Carthage.Tiles."=");

   type Tile_Info_Record is
      record
         Tile               : Carthage.Tiles.Tile_Type;
         Nearest_Seen       : Carthage.Tiles.Tile_Type;
         Nearest_Explored   : Carthage.Tiles.Tile_Type;
         Nearest_Controlled : Carthage.Tiles.Tile_Type;
         Controlled         : Boolean;
         Explored           : Boolean;
         Seen               : Boolean;
         Targeted           : Boolean;
      end record;

   type Tile_Info_Array is array (Tile_X, Tile_Y) of Tile_Info_Record;

   type Planet_Manager_Record is
     new Manager_Record with
      record
         Planet               : Carthage.Planets.Planet_Type;
         Ground_Asset_Manager : Carthage.Managers.Manager_Type;
         Controlled_Tiles     : List_Of_Tiles.List;
         Explored_Tiles       : List_Of_Tiles.List;
         Seen_Tiles           : List_Of_Tiles.List;
         Unseen_Tiles         : List_Of_Tiles.List;
         Target_Tiles         : List_Of_Tiles.List;
         Tile_Info            : Tile_Info_Array;
      end record;

   overriding procedure Create_Initial_State
     (Manager : in out Planet_Manager_Record);

   overriding procedure Load_State
     (Manager : in out Planet_Manager_Record);

   overriding procedure Execute
     (Manager : in out Planet_Manager_Record);

   --------------------------
   -- Create_Initial_State --
   --------------------------

   overriding procedure Create_Initial_State
     (Manager : in out Planet_Manager_Record)
   is
   begin
      Manager.Ground_Asset_Manager.Create_Initial_State;
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
      Manager.Ground_Asset_Manager :=
        Carthage.Managers.Assets.Ground_Asset_Manager
          (House, Planet);
      return new Planet_Manager_Record'(Manager);
   end Create_Planet_Manager;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Manager : in out Planet_Manager_Record)
   is
   begin
      for I in 1 .. 4 loop
         if not Manager.Target_Tiles.Is_Empty then
            Manager.Ground_Asset_Manager.Add_Request
              (Tile_Recon_Request
                 (Manager.Planet,
                  Manager.Target_Tiles.First_Element));
            Manager.Target_Tiles.Delete_First;
         end if;
      end loop;
      Manager.Ground_Asset_Manager.Execute;
   end Execute;

   ----------------
   -- Load_State --
   ----------------

   overriding procedure Load_State
     (Manager : in out Planet_Manager_Record)
   is
   begin
      Manager.Controlled_Tiles.Clear;
      Manager.Explored_Tiles.Clear;
      Manager.Seen_Tiles.Clear;
      Manager.Unseen_Tiles.Clear;

      for Y in Tile_Y loop
         for X in Tile_X loop
            declare
               Tile : constant Carthage.Tiles.Tile_Type :=
                        Manager.Planet.Tile (X, Y);
               Info : Tile_Info_Record :=
                        Tile_Info_Record'
                          (Tile               => Tile,
                           Nearest_Seen       => null,
                           Nearest_Explored   => null,
                           Nearest_Controlled => null,
                           Controlled         => False,
                           Explored           => False,
                           Seen               => False,
                           Targeted           => False);
            begin
               if Tile.Currently_Visible_To (Manager.House) then
                  Manager.Controlled_Tiles.Append (Tile);
                  Info.Nearest_Seen := Tile;
                  Info.Nearest_Explored := Tile;
                  Info.Nearest_Controlled := Tile;
               elsif Tile.Explored_By (Manager.House) then
                  Manager.Explored_Tiles.Append (Tile);
                  Info.Nearest_Explored := Tile;
                  Info.Nearest_Controlled := Tile;
               elsif Tile.Seen_By (Manager.House) then
                  Manager.Seen_Tiles.Append (Tile);
                  Info.Nearest_Seen := Tile;
               else
                  Manager.Unseen_Tiles.Append (Tile);
               end if;
               Manager.Tile_Info (X, Y) := Info;
            end;
         end loop;
      end loop;

      Manager.Target_Tiles := Manager.Unseen_Tiles;

      if Manager.Target_Tiles.Is_Empty then
         for Target of Manager.Seen_Tiles loop
            declare
               Position : constant Tile_Position :=
                            Target.Position;
               Info     : Tile_Info_Record renames
                            Manager.Tile_Info (Position.X, Position.Y);
            begin
               if not Info.Targeted then
                  declare
                     Good : Boolean := True;
                     Ns   : constant Carthage.Planets.Array_Of_Positions :=
                              Manager.Planet.Neighbours (Position);
                  begin
                     for N of Ns loop
                        declare
                           N_Info : Tile_Info_Record renames
                                      Manager.Tile_Info (N.X, N.Y);
                        begin
                           if N_Info.Targeted
                             or else N_Info.Controlled
                             or else N_Info.Explored
                           then
                              Good := False;
                              exit;
                           end if;
                        end;
                     end loop;

                     if Good then
                        Manager.House.Log
                          ("adding target: "
                           & Manager.Planet.Name
                           & Position.X'Img & Position.Y'Img
                           & " " & Target.Description);
                        for N of Ns loop
                           declare
                              N_Info : Tile_Info_Record renames
                                         Manager.Tile_Info (N.X, N.Y);
                           begin
                              N_Info.Targeted := True;
                           end;
                        end loop;
                        Info.Targeted := True;
                        Manager.Target_Tiles.Append (Target);
                     end if;
                  end;
               end if;
            end;
         end loop;
      end if;

      declare
         Arr : array (1 .. Natural (Manager.Target_Tiles.Length))
           of Carthage.Tiles.Tile_Type;
         Position : List_Of_Tiles.Cursor := Manager.Target_Tiles.First;
      begin
         for T of Arr loop
            T := List_Of_Tiles.Element (Position);
            List_Of_Tiles.Next (Position);
         end loop;

         for I in Arr'Range loop
            declare
               J : constant Positive :=
                     WL.Random.Random_Number (1, Arr'Length);
               T : constant Carthage.Tiles.Tile_Type :=
                     Arr (J);
            begin
               Arr (J) := Arr (I);
               Arr (I) := T;
            end;
         end loop;

         Manager.Target_Tiles.Clear;
         for T of Arr loop
            Manager.Target_Tiles.Append (T);
         end loop;
      end;

      Manager.Ground_Asset_Manager.Load_State;

   end Load_State;

end Carthage.Managers.Planets;
