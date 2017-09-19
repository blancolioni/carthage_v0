--  with Ada.Characters.Handling;
--  with Ada.Directories;
with Ada.Text_IO;

--  with WL.Images.FLC;

with Carthage.Structures;

with Carthage.Configure;
with Carthage.Import;

package body Carthage.UI is

   Background_Hex_Base      : WL.Images.Image_Type;
   Have_Background_Hex_Base : Boolean := False;

   ---------------------------
   -- Create_Background_Hex --
   ---------------------------

   function Create_Background_Hex
     (Background_Color : Carthage.Colours.Colour_Type)
      return WL.Images.Image_Type'Class
   is
      use WL.Images;
      use type Carthage.Colours.Colour_Element;
      Color : constant Image_Color :=
                (Red   => Color_Element (Background_Color.Red * 255.0),
                 Green => Color_Element (Background_Color.Green * 255.0),
                 Blue  => Color_Element (Background_Color.Blue * 255.0),
                 Alpha => Color_Element (Background_Color.Alpha * 255.0));
   begin
      return Image : WL.Images.Image_Type := Background_Hex_Base do
         for Y in 1 .. Image.Height loop
            for X in 1 .. Image.Width loop
               if Image.Color (X, Y) /= (0, 0, 0, 0) then
                  Image.Set_Color (X, Y, Color);
               end if;
            end loop;
         end loop;
      end return;
   end Create_Background_Hex;

   --------------------
   -- Load_Resources --
   --------------------

   procedure Load_Resources
     (Save_Image_Resource : not null access
        procedure (Resource_Name : String;
                   Image : WL.Images.Image_Type'Class))
   is
      procedure Load_Hexes
        (From_Bin_File    : String;
         World_Type       : String;
         Resource_Name_Fn : not null access
           function (World_Type : String;
                     Index      : Positive)
         return String);

      procedure Load_Icons
        (From_Bin_File    : String;
         Width, Height    : Natural;
         Resource_Name_Fn : not null access
           function (Index      : Natural)
         return String);

      ----------------
      -- Load_Hexes --
      ----------------

      procedure Load_Hexes
        (From_Bin_File    : String;
         World_Type       : String;
         Resource_Name_Fn : not null access
           function (World_Type : String;
                     Index      : Positive)
         return String)
      is

         Index : Natural := 0;

         function Planet_Hex_Width
           (Line : Natural)
            return Natural
         is (if Line <= 18
             then 28 + 2 * ((Line + 1) / 2)
             elsif Line in 19 .. 20
             then 48
             else 46 - 2 * ((Line - 21) / 2));

         procedure Save_Resource
           (Image : WL.Images.Image_Type'Class);

         -------------------------
         -- Save_Image_Resource --
         -------------------------

         procedure Save_Resource
           (Image : WL.Images.Image_Type'Class)
         is
         begin
            Index := Index + 1;
            Save_Image_Resource
              (Resource_Name_Fn (World_Type, Index),
               Image);

            if not Have_Background_Hex_Base then
               Background_Hex_Base := WL.Images.Image_Type (Image);
               Have_Background_Hex_Base := True;
            end if;

         end Save_Resource;

      begin

         Carthage.Import.Import_Hexes
           (Image_Width      => 48,
            Image_Height     => 40,
            Bin_File_Path    =>
              Carthage.Configure.Fading_Suns_Bin_File (From_Bin_File),
            Line_Width       => Planet_Hex_Width'Access,
            On_Load          =>
               Save_Resource'Access);
      end Load_Hexes;

      ----------------
      -- Load_Icons --
      ----------------

      procedure Load_Icons
        (From_Bin_File    : String;
         Width, Height    : Natural;
         Resource_Name_Fn : not null access
           function (Index      : Natural)
         return String)
      is

         Index : Natural := 0;

         procedure Save_Resource
           (Image : WL.Images.Image_Type'Class);

         -------------------
         -- Save_Resource --
         -------------------

         procedure Save_Resource
           (Image : WL.Images.Image_Type'Class)
         is
         begin
            Save_Image_Resource
              (Resource_Name_Fn (Index),
               Image);
            Index := Index + 1;
         end Save_Resource;

      begin

--           Carthage.Import.Import_Bin_File
--             (Image_Width      => Width,
--              Image_Height     => Height,
--              Bin_File_Path    =>
--                Carthage.Configure.Fading_Suns_Bin_File (From_Bin_File),
--              Destination_Path =>
--                Carthage.Paths.Config_File ("images/units"),
--              Base_Name        => "unit");

         Carthage.Import.Import_Icons
           (Icon_Width    => Width,
            Icon_Height   => Height,
            Bin_File_Path =>
              Carthage.Configure.Fading_Suns_Bin_File (From_Bin_File),
            On_Load       => Save_Resource'Access);

      end Load_Icons;

      -----------------------
      -- Structure_Name_Fn --
      -----------------------

      function Structure_Name_Fn
        (World_Type : String;
         Index      : Positive)
         return String
      is (World_Type
          & "-" & Carthage.Structures.Get (Index).Identifier
          & "-hex");

      ------------------
      -- Tile_Name_Fn --
      ------------------

      function Tile_Name_Fn
        (World_Type : String;
         Index      : Positive)
         return String
      is (World_Type & Integer'Image (-Index) & "-tile");

      ------------------
      -- Unit_Name_Fn --
      ------------------

      function Unit_Name_Fn
        (Index      : Natural)
         return String
      is ("unit" & Integer'Image (-Index));

   begin
      Ada.Text_IO.Put_Line ("loading city resources ...");
      Load_Hexes ("struct0", "normal", Structure_Name_Fn'Access);
      Load_Hexes ("struct1", "city", Structure_Name_Fn'Access);
      Load_Hexes ("struct2", "ice", Structure_Name_Fn'Access);
      Load_Hexes ("struct3", "jungle", Structure_Name_Fn'Access);
      Load_Hexes ("struct4", "barren", Structure_Name_Fn'Access);

      Ada.Text_IO.Put_Line ("loading tile resources ...");
      Load_Hexes ("efstile0", "normal", Tile_Name_Fn'Access);
      Load_Hexes ("efstile1", "city", Tile_Name_Fn'Access);
      Load_Hexes ("efstile2", "ice", Tile_Name_Fn'Access);
      Load_Hexes ("efstile3", "jungle", Tile_Name_Fn'Access);
      Load_Hexes ("efstile4", "barren", Tile_Name_Fn'Access);

      Ada.Text_IO.Put_Line ("loading counters ...");
      Load_Icons ("efsunit", 32, 32, Unit_Name_Fn'Access);

   end Load_Resources;

   ---------------------
   -- Set_Wizard_Mode --
   ---------------------

   procedure Set_Wizard_Mode
     (Enabled : Boolean)
   is
   begin
      Local_Wizard_Mode := Enabled;
   end Set_Wizard_Mode;

end Carthage.UI;
