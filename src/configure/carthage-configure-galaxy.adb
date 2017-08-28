with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

with Carthage.Terrain;
with Carthage.Tiles.Configure;
with Carthage.Planets.Configure;

package body Carthage.Configure.Galaxy is

   Map_Width  : constant := 50;
   Map_Height : constant := 48;

   End_Of_Section : constant := 16#FFFE#;

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

   procedure Read_Galaxy_File
     (File : in out File_Type);

   procedure Read_Planet
     (File           : in out File_Type;
      Galaxy_Version : Word_32);

   function Read_Jump_Gate
     (File : in out File_Type)
      return Boolean;
   --  Read a jump gate from the file.  Return False if there are
   --  no more gates

   -------------------
   -- Import_Galaxy --
   -------------------

   procedure Import_Galaxy
     (Path : String)
   is
      File : File_Type;
   begin
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

      loop
         Read_Planet (File, Version);

         declare
            End_Flag : Word_16;
         begin
            Read (File, End_Flag);
            exit when End_Flag = End_Of_Section;
            Set_Offset (File, Current_Offset (File) - 2);
         end;
      end loop;

      while Read_Jump_Gate (File) loop
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

   procedure Read_Planet
     (File           : in out File_Type;
      Galaxy_Version : Word_32)
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

      Ada.Text_IO.Put_Line ("loading: " & Name (1 .. Name_Length)
                            & " at" & X'Img & Y'Img);

      Read (File, Owner);
      Read (File, Sect);
      Read (File, Flags);
      Read (File, Tile_Set);

      Ada.Text_IO.Put_Line ("  owner:" & Owner'Img);
      Ada.Text_IO.Put_Line ("  tile set:" & Tile_Set'Img);

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

      begin
         Carthage.Planets.Configure.Import_Planet
           (Name (1 .. Name_Length),
            Natural (X), Natural (Y), Natural (Tile_Set),
            Create_Tile'Access);
      end;

   end Read_Planet;

end Carthage.Configure.Galaxy;
