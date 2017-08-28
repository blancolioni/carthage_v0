with Ada.Characters.Handling;
with Ada.Text_IO;

with Carthage.Cities.Create;
with Carthage.Planets.Surfaces;
with Carthage.Structures.Configure;

with Carthage.Galaxy.Configure;

package body Carthage.Planets.Configure is

   Current_Planet_Count : Natural := 0;

   function To_Coordinate (X : Natural) return Coordinate
   is (Coordinate (Float (X) / 50.0));

   procedure Create_Bonus_Tiles (Planet : in out Planet_Class);

   ----------------------
   -- Configure_Planet --
   ----------------------

   procedure Configure_Planet
     (Config : Tropos.Configuration)
   is

      function Identifier_To_Name (Identifier : String) return String;

      procedure Create (Planet : in out Planet_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Planet : in out Planet_Class) is
      begin

         Current_Planet_Count := Current_Planet_Count + 1;

         Planet.Create_With_Identity
           (Config.Config_Name);
         Planet.Set_Name (Identifier_To_Name (Config.Config_Name));

         Planet.Log ("creating");

         Planet.Index := Current_Planet_Count;
         Planet.Category :=
           Carthage.Worlds.Get
             (Config.Get ("category", "normal"));

         Planet.X := 0.5;
         Planet.Y := 0.5;

         Planet.Megacity := Config.Get ("megacity");

         Carthage.Planets.Surfaces.Create_Surface
           (Planet.Category, Planet.Tiles);

         Create_Bonus_Tiles (Planet);

         Planet.Log ("created " & Planet.Name);
         Ada.Text_IO.Put (" " & Planet.Name);
         Ada.Text_IO.Flush;
      end Create;

      ------------------------
      -- Identifier_To_Name --
      ------------------------

      function Identifier_To_Name (Identifier : String) return String is
         Result : String := Identifier;
         At_Word_Start : Boolean := True;
         function To_Upper (Ch : Character) return Character
                            renames Ada.Characters.Handling.To_Upper;
      begin
         for Ch of Result loop
            if At_Word_Start then
               Ch := To_Upper (Ch);
               At_Word_Start := False;
            elsif Ch = '-' then
               At_Word_Start := True;
            elsif Ch = '_' then
               Ch := ' ';
               At_Word_Start := True;
            end if;
         end loop;
         return Result;
      end Identifier_To_Name;

   begin
      Db.Create (Create'Access);
   end Configure_Planet;

   ------------------------
   -- Configure_Position --
   ------------------------

   procedure Configure_Position
     (Planet_Id : String;
      X, Y      : Coordinate)
   is

      procedure Set_Position (Planet : in out Planet_Class);

      ------------------
      -- Set_Position --
      ------------------

      procedure Set_Position (Planet : in out Planet_Class) is
      begin
         Planet.X := X;
         Planet.Y := Y;
      end Set_Position;

   begin
      if not Exists (Planet_Id) then
         raise Constraint_Error with
           "Configure_Position: no such planet: " & Planet_Id;
      end if;
      Db.Update
        (Get (Planet_Id).Reference, Set_Position'Access);
   end Configure_Position;

   ------------------------
   -- Create_Bonus_Tiles --
   ------------------------

   procedure Create_Bonus_Tiles (Planet : in out Planet_Class) is

      function Has_Land_Neighbour (Position : Tile_Position) return Boolean
      is (for some P of Planet.Neighbours (Position) =>
             not Planet.Tile (P).Is_Water);

      Count : Natural := 0;

   begin
      for Y in Tile_Y loop
         for X in Tile_X loop
            declare
               Position : constant Tile_Position := (X, Y);
               Tile : constant Carthage.Tiles.Tile_Type :=
                        Planet.Tile (Position);
            begin
               if not Tile.Is_Water
                 or else Has_Land_Neighbour (Position)
               then
                  declare
                     use Carthage.Structures;
                     Structure : constant Structure_Type :=
                                   Carthage.Structures.Configure.Random_Bonus
                                     (Planet.Category_Name,
                                      Tile.Base_Terrain.Identifier);
                  begin
                     if Structure /= null then
                        Carthage.Tiles.Set_City
                          (Tile,
                           Carthage.Cities.Create.New_City
                             (Planet    => Db.Reference (Planet.Reference),
                              Tile      => Tile,
                              Structure => Structure,
                              Owner     => null));
                        Count := Count + 1;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;

      Ada.Text_IO.Put (Count'Img);

   end Create_Bonus_Tiles;

   --------------------------
   -- Create_Surface_Graph --
   --------------------------

   procedure Create_Surface_Graph is

      subtype Offset_Range is Integer range -1 .. 1;

      type Offset_Array is array (1 .. 6) of Offset_Range;

      DX        : constant Offset_Array := (-1, 0, 1, 1, 0, -1);
      DY_Odd_X  : constant Offset_Array := (1, 1, 1, 0, -1, 0);
      DY_Even_X : constant Offset_Array := (0, 1, 0, -1, -1, -1);

   begin
      for Y in Tile_Y loop
         for X in Tile_X loop
            Surface_Graph.Append ((X, Y));
         end loop;
      end loop;

      for Y in Tile_Y loop
         for X in Tile_X loop
            for I in Offset_Array'Range loop
               declare
                  Odd_X : constant Boolean := X mod 2 = 1;
                  NX    : Integer := Integer (X) + DX (I);
                  NY    : constant Integer := Integer (Y)
                            + (if Odd_X then DY_Odd_X (I) else DY_Even_X (I));
               begin
                  if NY in 1 .. Planet_Height then
                     if NX = 0 then
                        NX := Planet_Width;
                     elsif NX = Planet_Width + 1 then
                        NX := 1;
                     end if;

                     Surface_Graph.Connect
                       (Index_Of ((X, Y)),
                        Index_Of ((Tile_X (NX), Tile_Y (NY))));
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end Create_Surface_Graph;

   ----------------------
   -- Import_Jump_Gate --
   ----------------------

   procedure Import_Jump_Gate
     (X1, Y1, X2, Y2 : Natural)
   is
      function Find_Nearest (X, Y : Natural) return Planet_Type;

      function Find_Nearest (X, Y : Natural) return Planet_Type is
         XX : constant Coordinate := To_Coordinate (X);
         YY : constant Coordinate := To_Coordinate (Y);
         Nearest_Planet : Planet_Type := null;
         Shortest_Distance : Float := Float'Last;

         procedure Check (Planet : Planet_Type);

         -----------
         -- Check --
         -----------

         procedure Check (Planet : Planet_Type) is
            D : constant Float :=
                  (Float (Planet.X) - Float (XX)) ** 2
                  + (Float (Planet.Y) - Float (YY)) ** 2;
         begin
            if Nearest_Planet = null or else D < Shortest_Distance then
               Nearest_Planet := Planet;
               Shortest_Distance := D;
            end if;
         end Check;

      begin
         Db.Scan (Check'Access);
         return Nearest_Planet;
      end Find_Nearest;

      From : constant Planet_Type := Find_Nearest (X1, Y1);
      To   : constant Planet_Type := Find_Nearest (X2, Y2);
   begin
      Carthage.Galaxy.Configure.Import_Gate
        (From.Index, To.Index);
   end Import_Jump_Gate;

   -------------------
   -- Import_Planet --
   -------------------

   procedure Import_Planet
     (Name        : String;
      X, Y        : Natural;
      Tile_Set    : Natural;
      Create_Tile : not null access
        function (X : Tile_X;
                  Y : Tile_Y)
      return Carthage.Tiles.Tile_Type)
   is

      function Identifier_To_Name (Identifier : String) return String;

      procedure Create (Planet : in out Planet_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Planet : in out Planet_Class) is
      begin

         Current_Planet_Count := Current_Planet_Count + 1;

         Planet.Create_With_Identity (Name);
         Planet.Set_Name (Identifier_To_Name (Name));

         Planet.Log ("creating");

         Planet.Index := Current_Planet_Count;

         Planet.Category :=
           Carthage.Worlds.Get
             (case Tile_Set is
                 when 0 => "normal",
                 when 1 => "city",
                 when 2 => "ice",
                 when 3 => "jungle",
                 when 4 => "barren",
                 when others =>
                    raise Constraint_Error with
                      Planet.Name & ": bad tileset:" & Tile_Set'Img);

         Planet.X := To_Coordinate (X);
         Planet.Y := To_Coordinate (Y);

         Planet.Megacity := Tile_Set = 1;

         for Y in Planet.Tiles'Range (2) loop
            for X in Planet.Tiles'Range (1) loop
               Planet.Tiles (X, Y) := Create_Tile (X, Y);
               Planet.Log (X'Img & Y'Img & ": "
                           & Planet.Tiles (X, Y).Description);
            end loop;
         end loop;

         Planet.Log ("created " & Planet.Name);
         Ada.Text_IO.Put (" " & Planet.Name);
         Ada.Text_IO.Flush;
      end Create;

      ------------------------
      -- Identifier_To_Name --
      ------------------------

      function Identifier_To_Name (Identifier : String) return String is
         Result        : String := Identifier;
         At_Word_Start : Boolean := True;
         function To_Upper (Ch : Character) return Character
                            renames Ada.Characters.Handling.To_Upper;
      begin
         for Ch of Result loop
            if At_Word_Start then
               Ch := To_Upper (Ch);
               At_Word_Start := False;
            elsif Ch = '-' then
               At_Word_Start := True;
            elsif Ch = '_' then
               Ch := ' ';
               At_Word_Start := True;
            end if;
         end loop;
         return Result;
      end Identifier_To_Name;

   begin
      Db.Create (Create'Access);
   end Import_Planet;

end Carthage.Planets.Configure;
