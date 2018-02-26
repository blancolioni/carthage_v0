private with Lui.Colours;
private with Carthage.Colours;

with Lui.Models;

with Carthage.Houses;

package Carthage.UI.Models is

   function Top_Model
     (House : not null access constant Carthage.Houses.House_Class)
     return Lui.Models.Object_Model;

private

   type Layout_Rectangle is
      record
         X, Y, Width, Height : Integer;
      end record;

   function Contains
     (Rectangle : Layout_Rectangle;
      X, Y      : Integer)
      return Boolean
   is (X in Rectangle.X .. Rectangle.X + Rectangle.Width - 1
       and then Y in Rectangle.Y .. Rectangle.Y + Rectangle.Height - 1);

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
      return Lui.Models.Object_Model
     with Pre => Have_Model (House, Key);

   procedure Save_Model
     (Model    : not null access Lui.Models.Root_Object_Model'Class;
      Class_Id : String);

end Carthage.UI.Models;
