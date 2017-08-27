with Carthage.UI.Models.Galaxy;

package body Carthage.UI.Models is

   Local_Top_Model : Carthage_Model := null;

   -------------------
   -- To_Lui_Colour --
   -------------------

   function To_Lui_Colour
     (Colour : Carthage.Colours.Colour_Type)
      return Lui.Colours.Colour_Type
   is
      use Carthage.Colours;
      use Lui.Colours;
   begin
      return Apply_Alpha
        (To_Colour
           (Red   => Colour_Byte (Colour.Red * 255.0),
            Green => Colour_Byte (Colour.Green * 255.0),
            Blue  => Colour_Byte (Colour.Blue * 255.0)),
         Lui.Unit_Real (Colour.Alpha));
   end To_Lui_Colour;

   ---------------
   -- Top_Model --
   ---------------

   function Top_Model return Carthage_Model is
      use type Lui.Models.Object_Model;
   begin
      if Local_Top_Model = null then
         Local_Top_Model := Carthage.UI.Models.Galaxy.Galaxy_Model;
      end if;

      return Local_Top_Model;
   end Top_Model;

end Carthage.UI.Models;
