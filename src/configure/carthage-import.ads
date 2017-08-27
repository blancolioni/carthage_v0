with WL.Images;

with Carthage.Colours;

package Carthage.Import is

   procedure Import_Bin_File
     (Image_Width      : Natural;
      Image_Height     : Natural;
      Bin_File_Path    : String;
      Destination_Path : String;
      Base_Name        : String);

   procedure Import_Icons
     (Icon_Width      : Natural;
      Icon_Height     : Natural;
      Bin_File_Path    : String;
      On_Load          : not null access
        procedure (Image : WL.Images.Image_Type'Class));

   procedure Import_Hexes
     (Image_Width      : Natural;
      Image_Height     : Natural;
      Bin_File_Path    : String;
      Line_Width       : not null access
        function (Line : Natural) return Natural;
      On_Load          : not null access
        procedure (Image : WL.Images.Image_Type'Class));

   function Palette_Colour
     (Palette_Index : Natural)
      return Carthage.Colours.Colour_Type;

   procedure Scan_Settings
     (Settings : String;
      Process  : not null access
        procedure (Index : Positive;
                   Setting : String));

   type Numeric_Settings is array (Positive range <>) of Integer;

   procedure Scan_Settings
     (Settings : String;
      Result   : in out Numeric_Settings);

end Carthage.Import;
