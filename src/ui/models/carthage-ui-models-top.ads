with Lui.Rendering;

with Carthage.Resources;
with Carthage.Stacks;

package Carthage.UI.Models.Top is

   type Top_Carthage_Model is
     new Lui.Models.Root_Object_Model with private;

   type Top_Model is access all Top_Carthage_Model'Class;

   function Create_Top_Model
     (House : Carthage.Houses.House_Type)
      return Top_Model;

   overriding procedure Render
     (Model    : in out Top_Carthage_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      Layer    : Lui.Render_Layer);

   overriding procedure Resize
     (Item          : in out Top_Carthage_Model;
      Width, Height : Natural);

   procedure Initialize_Model
     (Model : not null access Top_Carthage_Model'Class;
      House              : not null access constant
        Carthage.Houses.House_Record'Class);

   function House
     (Model : Top_Carthage_Model'Class)
      return Carthage.Houses.House_Type;

   function Wizard_Mode
     (Model : Top_Carthage_Model'Class)
      return Boolean;

private

   type Resource_Layout_Record is
      record
         Rectangle : Lui.Layout_Rectangle;
      end record;

   type Resource_Layout_Array is
     array (Carthage.Resources.Resource_Index range <>)
     of Resource_Layout_Record;

   type Top_Carthage_Model is
     new Lui.Models.Root_Object_Model with
      record
         House                 : Carthage.Houses.House_Type;
         Stack                 : Carthage.Stacks.Stack_Type;
         Wizard                : Boolean := False;
         Layout_Loaded         : Boolean := False;
         Left_Toolbar_Layout   : Lui.Layout_Rectangle;
         Top_Toolbar_Layout    : Lui.Layout_Rectangle;
         Bottom_Toolbar_Layout : Lui.Layout_Rectangle;
         Main_Rectangle        : Lui.Layout_Rectangle;
         Mini_Map_Layout       : Lui.Layout_Rectangle;
         Status_Layout         : Lui.Layout_Rectangle;
         Selected_Stack_Layout : Lui.Layout_Rectangle;
         Sidebar_Icon_Size     : Positive := 64;
         Resource_Layout       : access Resource_Layout_Array;
      end record;

   function House
     (Model : Top_Carthage_Model'Class)
      return Carthage.Houses.House_Type
   is (Model.House);

   function Wizard_Mode
     (Model : Top_Carthage_Model'Class)
      return Boolean
   is (Model.Wizard);

end Carthage.UI.Models.Top;
