with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

with Tropos.Reader;

with Carthage.Assets.Create;
with Carthage.Cities.Create;

with Carthage.Houses;
with Carthage.Stacks;
with Carthage.Structures;
with Carthage.Terrain;
with Carthage.Tiles.Configure;
with Carthage.Planets.Configure;
with Carthage.Resources;
with Carthage.Stacks.Create;
with Carthage.Units;

with Carthage.Configure;
with Carthage.Options;
with Carthage.Paths;

package body Carthage.Import.Galaxy is

   Initial_State_Config : Tropos.Configuration;
   Initial_Stock_Config : Tropos.Configuration;

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

   type File_City is
      record
         Planet          : Word_16;
         X, Y            : Word_16;
         C_Type          : Word_16;
         Owner           : Word_16;
         Unknown_1       : Word_16;
         Ruin_Type       : Word_16;
         Production_Info : Word_16;
         Turns_Left      : Word_16;
         City_Info       : Word_16;
         Unknown_2       : Word_16;
         Unit_Loyalty    : Word_16;
         Loyalty         : Word_16;
         Stack_Info      : Word_16;
         Used_Unit_Level : Word_16;
         Tech_Type       : Word_16;
         Health          : Word_16;
         Sect            : Word_16;
         Flags           : Word_32;
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

   Road_Mask       : constant := 2#1011#;
   Road_Tile       : constant := 2#1011#;

   Tree_Mask       : constant := 2#1000#;

   Unit_On_Ground_Mask : constant := 16#0001#;
   Unit_Sentry_Mask    : constant := 16#0004#;
   Unit_Cargo_Mask     : constant := 16#0040#;

   House_Map  : array (Word_8) of Carthage.Houses.House_Type;
   Planet_Map : array (Word_8) of Carthage.Planets.Planet_Type;
   Next_Planet : Word_8 := 0;

   procedure Read_Galaxy_File
     (File : in out File_Type);

   function Read_City
     (File : in out File_Type)
      return Boolean;
   --  Read a city from the file.  Return False if there are
   --  no more cities

   function Read_Jump_Gate
     (File : in out File_Type)
      return Boolean;
   --  Read a jump gate from the file.  Return False if there are
   --  no more gates

   function Read_Planet
     (File           : in out File_Type;
      Galaxy_Version : Word_32)
      return Boolean;

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
      Carthage.Configure.Load_Standard_Houses;

      House_Map (0) := Carthage.Houses.Get ("li-halan");
      House_Map (1) := Carthage.Houses.Get ("hazat");
      House_Map (2) := Carthage.Houses.Get ("decados");
      House_Map (3) := Carthage.Houses.Get ("hawkwood");
      House_Map (4) := Carthage.Houses.Get ("al-malik");
      House_Map (5) := Carthage.Houses.Get ("league");
      House_Map (6) := Carthage.Houses.Get ("church");
      House_Map (7) := Carthage.Houses.Get ("symbiot");
      House_Map (8) := Carthage.Houses.Get ("vau");
      House_Map (9) := Carthage.Houses.Get ("imperial");
      House_Map (10) := Carthage.Houses.Get ("fleet");
      House_Map (11) := Carthage.Houses.Get ("stigmata");
      House_Map (12) := Carthage.Houses.Get ("spy");
      House_Map (13) := Carthage.Houses.Get ("rebel");

      Initial_State_Config :=
        Tropos.Reader.Read_Config
          (Carthage.Paths.Config_File ("initial-state.txt"));

      Initial_Stock_Config :=
        Initial_State_Config.Child ("stock");

      Ada.Text_IO.Put_Line
        ("importing galaxy from: " & Path);

      Open (File, In_File, Path);
      Read_Galaxy_File (File);
      Close (File);

      if not Ada.Directories.Exists
        (Carthage.Paths.Config_File
           ("localisation/english/planets.txt"))
      then
         declare
            use Ada.Text_IO;

            File : Ada.Text_IO.File_Type;

            procedure Write (Planet : Carthage.Planets.Planet_Type);

            -----------
            -- Write --
            -----------

            procedure Write (Planet : Carthage.Planets.Planet_Type) is
               Name : String := Planet.Identifier;
               First : Boolean := True;
            begin
               for Ch of Name loop
                  if First then
                     Ch := Ada.Characters.Handling.To_Upper (Ch);
                     First := False;
                  elsif Ch = '-' then
                     Ch := ' ';
                     First := True;
                  end if;
               end loop;

               Put_Line (File,
                         Planet.Identifier & ","
                         & Name (8 .. Name'Last));
            end Write;

         begin
            Create (File, Out_File,
                    Carthage.Paths.Config_File
                      ("localisation/english/planets.txt"));
            Carthage.Planets.Scan (Write'Access);
            Close (File);
         end;
      end if;

   end Import_Galaxy;

   ---------------
   -- Read_City --
   ---------------

   function Read_City
     (File : in out File_Type)
      return Boolean
   is

      City          : File_City;
      Check_Section : Word_16;
   begin
      Read (File, Check_Section);
      if Check_Section = End_Of_Section then
         return False;
      end if;

      City.Planet := Check_Section;
      Read (File, City.X);
      Read (File, City.Y);
      Read (File, City.C_Type);
      Read (File, City.Owner);
      Read (File, City.Unknown_1);
      Read (File, City.Ruin_Type);
      Read (File, City.Production_Info);
      Read (File, City.Turns_Left);
      Read (File, City.City_Info);
      Read (File, City.Unknown_2);
      Read (File, City.Unit_Loyalty);
      Read (File, City.Loyalty);
      Read (File, City.Stack_Info);
      Read (File, City.Used_Unit_Level);
      Read (File, City.Tech_Type);
      Read (File, City.Health);
      Read (File, City.Sect);
      Read (File, City.Flags);

      declare
         Planet   : constant Carthage.Planets.Planet_Type :=
                      Planet_Map (Word_8 (City.Planet));
         Tile     : constant Carthage.Tiles.Tile_Type :=
                      Planet.Tile
                        ((Tile_X (City.X + 1),
                         Tile_Y
                           (Word_16'Min (City.Y / 2 + 1, Planet_Height))));
         Structure : constant Carthage.Structures.Structure_Type :=
                       Carthage.Structures.Get
                         (Natural (City.C_Type) + 1);

         New_City : constant Carthage.Cities.City_Type :=
                       Carthage.Cities.Create.New_City
                         (Planet    => Planet,
                          Tile      => Tile,
                          Structure => Structure,
                          Owner     => House_Map (Word_8 (City.Owner)));
      begin

         Planet.Update.Add_City (New_City);

         if Initial_Stock_Config.Contains (Structure.Identifier) then
            declare
               procedure Update (Rec : in out Carthage.Cities.City_Class);

               ------------
               -- Update --
               ------------

               procedure Update (Rec : in out Carthage.Cities.City_Class) is
               begin
                  for Item of
                    Initial_Stock_Config.Child
                      (Structure.Identifier)
                  loop
                     Rec.Add
                       (Carthage.Resources.Get (Item.Config_Name),
                        Item.Value);
                  end loop;
               end Update;

            begin
               Carthage.Cities.Update_City (New_City, Update'Access);
            end;
         end if;
      end;

      return True;

   end Read_City;

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

      Ada.Text_IO.New_Line;

      while Read_Jump_Gate (File) loop
         null;
      end loop;

      while Read_Unit (File) loop
         null;
      end loop;

      while Read_City (File) loop
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
      Name_Start       : Positive := 1;
      Name_End         : Natural := 0;
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
               Name_End := Name_End + 1;
            end if;
         end loop;
      end;

      --  Madoc starts with a space!
      while Name (Name_Start) = ' ' loop
         Name_Start := Name_Start + 1;
      end loop;

      declare
         use Ada.Characters.Handling;
      begin
         for I in Name_Start .. Name_End loop
            if Is_Upper (Name (I)) then
               Name (I) := To_Lower (Name (I));
            elsif Name (I) = ' ' then
               Name (I) := '-';
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

      if Carthage.Options.Trace_Planet_Import then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("--- " & Name (Name_Start .. Name_End) & " ---");
      end if;

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
                    "while importing " & Name (Name_Start .. Name_End)
                    & ": no such terrain id: " & Id;
            end Add;

            Index : constant Positive := To_File_Map (X, Y);
            Flag  : Word_32  := Map (Index);
            Road  : constant Boolean := (Flag and Road_Mask) = Road_Tile;
         begin

            if Carthage.Options.Trace_Planet_Import then
               if X = Tile_X'First then
                  Ada.Text_IO.Put (Y'Img & ":");
                  Ada.Text_IO.Set_Col (11);
               end if;

               Ada.Text_IO.Put (" " & Hex_Image (Flag));

               if X = Tile_X'Last then
                  Ada.Text_IO.New_Line;
               end if;
            end if;

            if Road then
               Flag := Flag / 2 ** 9;
            end if;

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
               Terrain  => Ts (1 .. Count),
               Road     => Road,
               River    => False);
         end Create_Tile;

         Planet : constant Carthage.Planets.Planet_Type :=
                    Carthage.Planets.Configure.Import_Planet
                      (Ada.Characters.Handling.To_Lower
                         (Name (Name_Start .. Name_End)),
                       Natural (X), Natural (Y), Natural (Tile_Set),
                       Create_Tile'Access);
      begin
         Planet_Map (Next_Planet) := Planet;
         Next_Planet := Next_Planet + 1;
      end;

      if Carthage.Options.Trace_Planet_Import then
         Ada.Text_IO.New_Line;
      end if;

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
                       then Planet.Stack (Asset.Owner)
                       elsif Tile.Has_Stack
                       then Tile.Stack
                       else Carthage.Stacks.Create.New_Ground_Stack
                         (Asset.Owner, Planet, Tile));
      begin
         if In_Space then
            Carthage.Stacks.Add_Asset (Stack, Asset);
         else
            declare
               use type Carthage.Stacks.Asset_Count;
            begin
               if Stack.Count = 0 then
                  if Carthage.Options.Trace_Unit_Import then
                     Ada.Text_IO.Put_Line
                       ("new stack on "
                        & Planet.Name
                        & " " & Tile.Description
                        & " at" & Tile.Position.X'Img & Tile.Position.Y'Img);
                  end if;
               end if;
            end;
            Carthage.Stacks.Add_Asset (Stack, Asset);
            if not Tile.Has_Stack then
               Tile.Update.Set_Stack (Stack);
            end if;
         end if;
      end;

      return True;

   end Read_Unit;

end Carthage.Import.Galaxy;
