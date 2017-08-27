with WL.Images;

with Carthage.Colours;

package Carthage.UI is

   procedure Load_Resources
     (Save_Image_Resource : not null access
        procedure (Resource_Name : String;
                   Image         : WL.Images.Image_Type'Class));

   function Create_Background_Hex
     (Background_Color : Carthage.Colours.Colour_Type)
      return WL.Images.Image_Type'Class;

end Carthage.UI;
