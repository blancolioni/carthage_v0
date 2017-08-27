private with Lui.Colours;
private with Carthage.Colours;

with Lui.Models;

package Carthage.UI.Models is

   type Root_Carthage_Model is
     abstract new Lui.Models.Root_Object_Model with private;

   procedure Reload
     (Model : in out Root_Carthage_Model)
   is null;

   type Carthage_Model is access all Root_Carthage_Model'Class;

   function Top_Model return Carthage_Model;

private

   type Root_Carthage_Model is
     abstract new Lui.Models.Root_Object_Model with
      record
         null;
      end record;

   function To_Lui_Colour
     (Colour : Carthage.Colours.Colour_Type)
      return Lui.Colours.Colour_Type;

end Carthage.UI.Models;
