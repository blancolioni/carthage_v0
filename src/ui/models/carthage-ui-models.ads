private with Lui.Colors;
private with Carthage.Colors;

with Lui.Models;

with Carthage.Houses;

package Carthage.UI.Models is

   type Root_Carthage_Model is
     abstract new Lui.Models.Root_Object_Model with private;

   type Carthage_Model is access all Root_Carthage_Model'Class;

   function House
     (Model : Root_Carthage_Model'Class)
      return Carthage.Houses.House_Type;

   function Minimap_Model
     (Model : Root_Carthage_Model'Class)
      return Lui.Models.Object_Model;

private

   type Root_Carthage_Model is
     abstract new Lui.Models.Root_Object_Model with
      record
         House   : Carthage.Houses.House_Type;
         Minimap : Lui.Models.Object_Model;
      end record;

   function House
     (Model : Root_Carthage_Model'Class)
      return Carthage.Houses.House_Type
   is (Model.House);

   function Minimap_Model
     (Model : Root_Carthage_Model'Class)
      return Lui.Models.Object_Model
   is (Model.Minimap);

   function To_Lui_Color
     (Color : Carthage.Colors.Color_Type)
      return Lui.Colors.Color_Type;

   function Have_Model
     (House : not null access constant Carthage.Houses.House_Record'Class;
      Key   : String)
      return Boolean;

   function Get_Model
     (House : not null access constant Carthage.Houses.House_Record'Class;
      Key   : String)
      return Lui.Models.Object_Model
     with Pre => Have_Model (House, Key);

   procedure Save_Model
     (Model    : not null access Lui.Models.Root_Object_Model'Class;
      Class_Id : String);

end Carthage.UI.Models;
