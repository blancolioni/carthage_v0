with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

with Carthage.Assets.Create;
with Carthage.Houses;
with Carthage.Stacks;
with Carthage.Terrain;
with Carthage.Tiles.Configure;
with Carthage.Planets.Configure;
with Carthage.Stacks.Create;
with Carthage.Units;

package body Carthage.Configure.Galaxy is

   type File_Unit is
      record
         Planet          : Word_16;
         X, Y            : Word_16;
         Owner           : Word_8;
         U_Type          : Word_8;
         Unknown_1       : Word_8;
         Loyalty         : Word_8;
         Orders          : Word_16;
         Experience      : Word_8;
         Move_Points     : Word_8;
         Relic           : Word_8;
         Quantity        : Word_16;
         Health          : Word_8;
         Sect            : Word_8;
         Unknown_2       : Word_8;
         Unit_Number     : Word_32;
         Flags           : Word_32;
         Used_Unit_Type  : Word_8;
         Used_Unit_Level : Word_8;
         Camouflage      : Word_8;
         Destination_X   : Word_8;
         Destination_Y   : Word_8;
         Unknown_3       : Word_8;
         Task_Force      : Word_8;
         Unknown_4       : Word_16;
         Wait_Level      : Word_8;
      end record;

   Map_Width  : constant := 50;
   Map_Height : constant := 48;

   End_Of_Section : constant := 16#FFFE#;
   End_Of_Group   : constant := 16#FFFD#;

   Planet_Orbit_Stacks : constant := 8;
   Planet_Name_Length  : constant := 32;

   Planet_File_Width   : constant := Planet_Width;
   Planet_File_Height  : constant := 65;

   type Tile_Land_Bits is mod 2 ** 3;
   Ocean_Mask      : constant := 2#1111#;
   Ocean_Tile      : constant := 2#0000#;

   Land_Mask       : constant := 2#0111#;
   Grass_Tile      : constant := 2#0001#;
   Arid_Grass_Tile : constant := 2#0010#;
   Desert_Tile     : constant := 2#0011#;
   Ice_Tile        : constant := 2#0100#;
   Tundra_Tile     : constant := 2#0101#;
   Mountain_Tile   : constant := 2#0110#;
   Hill_Tile       : constant := 2#0111#;

   Tree_Mask       : constant := 2#1000#;

   Unit_On_Ground_Mask : constant := 16#0001#;
   Unit_Sentry_Mask    : constant := 16#0004#;
   Unit_Cargo_Mask     : constant := 16#0040#;

   House_Map  : array (Word_8) of Carthage.Houses.House_Type;
   Planet_Map : array (Word_8) of Carthage.Planets.Planet_Type;
   Next_Planet : Word_8 := 0;

   procedure Read_Galaxy_File
     (File : in out File_Type);

   function Read_Planet
     (File           : in out File_Type;
      Galaxy_Version : Word_32)
      return Boolean;

   function Read_Jump_Gate
     (File : in out File_Type)
      return Boolean;
   --  Read a jump gate from the file.  Return False if there are
   --  no more gates

   function Read_Unit
     (File : in out File_Type)
      return Boolean;
   --  Read a unit from the file.  Return False if there are
   --  no more units

   -------------------
   -- Import_Galaxy --
   -------------------

   procedure Import_Galaxy
     (Path : String)
   is
      File : File_Type;
   begin

      --  Galaxy file has no houses, so pull them from the standard scenario
      Load_Standard_Houses;

      House_Map (0) := Carthage.Houses.Get ("li-halan");
      House_Map (1) := Carthage.Houses.Get ("hazat");
      House_Map (2) := Carthage.Houses.Get ("decados");
      House_Map (3) := Carthage.Houses.Get ("hawkwood");
      House_Map (4) := Carthage.Houses.Get ("al-malik");
      House_Map (5) := Carthage.Houses.Get ("league");
      House_Map (6) := Carthage.Houses.Get ("church");
      House_Map (7) := Carthage.Houses.Get ("symbiots");
      House_Map (8) := Carthage.Houses.Get ("vau");
      House_Map (9) := Carthage.Houses.Get ("imperial");
      House_Map (10) := Carthage.Houses.Get ("fleet");
      House_Map (11) := Carthage.Houses.Get ("stigmata");
      House_Map (12) := Carthage.Houses.Get ("spy");
      House_Map (13) := Carthage.Houses.Get ("neutral");
      House_Map (14) := Carthage.Houses.Get ("rebels");

      Ada.Text_IO.Put_Line
        ("importing galaxy from: " & Path);

      Open (File, In_File, Path);
      Read_Galaxy_File (File);
      Close (File);
   end Import_Galaxy;

   ----------------------
   -- Read_Galaxy_File --
   ----------------------

   procedure Read_Galaxy_File
     (File : in out File_Type)
   is
      Version    : Word_32;
      Unit_Count : Word_32;
      Map_Tiles  : array (1 .. Map_Width, 1 .. Map_Height) of Word_32;

   begin
      Read (File, Version);

      Ada.Text_IO.Put_Line ("reading galaxy version" & Version'Img);

      Read (File, Unit_Count);
      for Y in Map_Tiles'Range (2) loop
         for X in Map_Tiles'Range (1) loop
            Read (File, Map_Tiles (X, Y));
         end loop;
      end loop;

      while Read_Planet (File, Version) loop
         null;
      end loop;

      while Read_Jump_Gate (File) loop
         null;
      end loop;

      while Read_Unit (File) loop
         null;
      end loop;

   end Read_Galaxy_File;

   --------------------
   -- Read_Jump_Gate --
   --------------------

   function Read_Jump_Gate
     (File : in out File_Type)
      return Boolean
   is
      X1, Y1, X2, Y2 : Word_16;
      Flags          : Word_32;
   begin
      Read (File, X1);
      if X1 = End_Of_Section then
         return False;
      end if;

      Read (File, Y1);
      Read (File, X2);
      Read (File, Y2);
      Read (File, Flags);

      Carthage.Planets.Configure.Import_Jump_Gate
        (Natural (X1), Natural (Y1), Natural (X2), Natural (Y2));

      return True;

   end Read_Jump_Gate;

   -----------------
   -- Read_Planet --
   -----------------

   function Read_Planet
     (File           : in out File_Type;
      Galaxy_Version : Word_32)
      return Boolean
   is
      X, Y, R          : Word_16;
      Orbiting_Stacks  : array (1 .. Planet_Orbit_Stacks) of Word_32;
      Name             : String (1 .. Planet_Name_Length);
      Name_Length      : Natural := 0;
      Owner            : Word_16;
      Sect             : Word_16;
      Flags            : Word_32;
      Tile_Set         : Word_16;
      Map              : array
        (1 .. Planet_File_Width * Planet_File_Height) of Word_32;

      To_File_Map      : array (Tile_X, Tile_Y) of Positive;

   begin

      Read (File, X);

      if X = End_Of_Section then
         return False;
      end if;

      Read (File, Y);
      Read (File, R);

      for Stack of Orbiting_Stacks loop
         Read (File, Stack);
      end loop;

      declare
         Done : Boolean := False;
         X    : Word_8;
      begin
         for Ch of Name loop
            Read (File, X);
            Done := Done or else X = 0;
            Ch := Character'Val (X);
            if not Done then
               Name_Length := Name_Length + 1;
            end if;
         end loop;
      end;

      Read (File, Owner);
      Read (File, Sect);
      Read (File, Flags);
      Read (File, Tile_Set);

      if Galaxy_Version < 961024 then
         Skip (File, 12);
      end if;

      Skip (File, 2);

      for Hex_Flags of Map loop
         Read (File, Hex_Flags);
      end loop;

      for File_X in 1 .. Planet_File_Width loop
         declare
            Row : Natural := 0;
         begin
            for File_Y in 1 .. Planet_File_Height - 1 loop
               declare
                  Index : constant Positive :=
                            Planet_File_Height * (File_X - 1) + File_Y;
               begin
                  if (File_X mod 2 = 1 and then File_Y mod 2 = 0)
                    or else (File_X mod 2 = 0 and then File_Y mod 2 = 1)
                  then
                     Row := Row + 1;
                     To_File_Map (Tile_X (File_X), Tile_Y (Row)) := Index;
                  end if;
               end;
            end loop;
         end;
      end loop;

      declare
         function Create_Tile
           (X : Tile_X;
            Y : Tile_Y)
            return Carthage.Tiles.Tile_Type;

         -----------------
         -- Create_Tile --
         -----------------

         function Create_Tile
           (X : Tile_X;
            Y : Tile_Y)
            return Carthage.Tiles.Tile_Type
         is
            Ts : Carthage.Tiles.Configure.Terrain_Array (1 .. 10);
            Count : Natural := 0;

            procedure Add (Id : String);

            ---------
            -- Add --
            ---------

            procedure Add (Id : String) is
            begin
               Count := Count + 1;
               Ts (Count) := Carthage.Terrain.Get (Id);
            exception
               when others =>
                  raise Constraint_Error with
                    "while importing " & Name (1 .. Name_Length)
                    & ": no such terrain id: " & Id;
            end Add;

            Index : constant Positive := To_File_Map (X, Y);
            Flag  : constant Word_32  := Map (Index);
         begin
            if (Flag and Ocean_Mask) = Ocean_Tile then
               Add ("ocean");
            else
               case Tile_Land_Bits (Flag and Land_Mask) is
                  when 0 =>
                     null;  --  forest-only tile
                  when Grass_Tile =>
                     Add ("grass");
                  when Arid_Grass_Tile =>
                     Add ("arid_grass");
                  when Desert_Tile =>
                     Add ("desert");
                  when Ice_Tile =>
                     Add ("ice");
                  when Tundra_Tile =>
                     Add ("tundra");
                  when Mountain_Tile =>
                     Add ("mountain");
                  when Hill_Tile =>
                     Add ("hill");
               end case;

               if (Flag and Tree_Mask) = Tree_Mask then
                  Add ("tree");
               end if;
            end if;

            return Carthage.Tiles.Configure.Create_Tile
              (Index    => Positive (X) + Planet_Width * (Natural (Y - 1)),
               Position => (X, Y),
               Terrain  => Ts (1 .. Count));
         end Create_Tile;

         Planet : constant Carthage.Planets.Planet_Type :=
                    Carthage.Planets.Configure.Import_Planet
                      (Name (1 .. Name_Length),
                       Natural (X), Natural (Y), Natural (Tile_Set),
                       Create_Tile'Access);
      begin
         Planet_Map (Next_Planet) := Planet;
         Next_Planet := Next_Planet + 1;
      end;

      return True;

   end Read_Planet;

   ---------------
   -- Read_Unit --
   ---------------

   function Read_Unit
     (File : in out File_Type)
      return Boolean
   is

      Unit : File_Unit;
      Check_Section : Word_16;
   begin
      Read (File, Check_Section);
      if Check_Section = End_Of_Section then
         return False;
      elsif Check_Section = End_Of_Group then
         Read (File, Check_Section);
         if Check_Section = End_Of_Section then
            return False;
         end if;
      end if;

      Unit.Planet := Check_Section;
      Read (File, Unit.X);
      Read (File, Unit.Y);
      Read (File, Unit.Owner);
      Read (File, Unit.U_Type);
      Read (File, Unit.Unknown_1);
      Read (File, Unit.Loyalty);
      Read (File, Unit.Orders);
      Read (File, Unit.Experience);
      Read (File, Unit.Move_Points);
      Read (File, Unit.Relic);
      Read (File, Unit.Quantity);
      Read (File, Unit.Health);
      Read (File, Unit.Sect);
      Read (File, Unit.Unknown_2);
      Read (File, Unit.Unit_Number);
      Read (File, Unit.Flags);
      Read (File, Unit.Used_Unit_Type);
      Read (File, Unit.Used_Unit_Level);
      Read (File, Unit.Camouflage);
      Read (File, Unit.Destination_X);
      Read (File, Unit.Destination_Y);
      Read (File, Unit.Unknown_3);
      Read (File, Unit.Task_Force);
      Read (File, Unit.Unknown_4);
      Read (File, Unit.Wait_Level);

      declare
         Asset : constant Carthage.Assets.Asset_Type :=
                   Carthage.Assets.Create.New_Asset
                     (Unit    =>
                         Carthage.Units.Get (Natural (Unit.U_Type)),
                      Owner   => House_Map (Unit.Owner),
                      XP      =>
                        Carthage.Assets.Asset_Experience'Val
                          (Unit.Experience),
                      Loyalty =>
                        Carthage.Assets.Asset_Loyalty (Unit.Loyalty),
                      Health  =>
                        Carthage.Assets.Asset_Health
                          (Unit.Health));
         In_Space : constant Boolean :=
                      (Unit.Flags and Unit_On_Ground_Mask) = 0;
         Sentry   : constant Boolean :=
                      (Unit.Flags and Unit_Sentry_Mask)
                      = Unit_Sentry_Mask
           with Unreferenced;
         Cargo    : constant Boolean :=
                      (Unit.Flags and Unit_Cargo_Mask)
                      = Unit_Cargo_Mask;
         Planet : constant Carthage.Planets.Planet_Type :=
                    Planet_Map (Word_8 (Unit.Planet));
         Tile     : constant Carthage.Tiles.Tile_Type :=
                      (if In_Space or else Cargo then null
                       else Planet.Tile
                         ((Tile_X (Unit.X + 1),
                          Tile_Y (Unit.Y / 2 + 1))));
         Stack    : constant Carthage.Stacks.Stack_Type :=
                      (if In_Space
                       then null
                       elsif Tile.Has_Stack
                       then Tile.Stack
                       else Carthage.Stacks.Create.New_Ground_Stack
                         (Asset.Owner, Planet, Tile));
      begin
         if not In_Space then
            Carthage.Stacks.Add_Asset (Stack, Asset);
            if not Tile.Has_Stack then
               Carthage.Tiles.Set_Stack (Tile, Stack);
            end if;
         end if;
      end;

      return True;

   end Read_Unit;

end Carthage.Configure.Galaxy;
