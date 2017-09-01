private with Lui.Colours;
private with Carthage.Colours;

with Lui.Models;

with Carthage.Houses;

package Carthage.UI.Models is

   type Root_Carthage_Model is
     abstract new Lui.Models.Root_Object_Model with private;

   procedure Reload
     (Model : in out Root_Carthage_Model)
   is null;

   type Carthage_Model is access all Root_Carthage_Model'Class;

   function Top_Model
     (House : not null access constant Carthage.Houses.House_Class)
     return Carthage_Model;

private

   type Root_Carthage_Model is
     abstract new Lui.Models.Root_Object_Model with
      record
         House  : Carthage.Houses.House_Type;
         Wizard : Boolean := False;
      end record;

   function To_Lui_Colour
     (Colour : Carthage.Colours.Colour_Type)
      return Lui.Colours.Colour_Type;

   function Have_Model
     (House : not null access constant Carthage.Houses.House_Record'Class;
      Key   : String)
      return Boolean;

   function Get_Model
     (House : not null access constant Carthage.Houses.House_Record'Class;
      Key   : String)
      return Carthage_Model
     with Pre => Have_Model (House, Key);

   procedure Set_Model
     (House : not null access constant Carthage.Houses.House_Record'Class;
      Key   : String;
      Model : not null access Root_Carthage_Model'Class)
     with Pre => not Have_Model (House, Key),
     Post => Have_Model (House, Key);

end Carthage.UI.Models;
