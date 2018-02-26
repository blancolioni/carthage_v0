with Lui.Rendering;

with Carthage.Resources;
with Carthage.Stacks;

private package Carthage.UI.Models.Top is

   type Root_Carthage_Model is
     abstract new Lui.Models.Root_Object_Model with private;

   type Carthage_Model is access all Root_Carthage_Model'Class;

   overriding procedure Render
     (Model    : in out Root_Carthage_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding procedure Resize
     (Item          : in out Root_Carthage_Model;
      Width, Height : Natural);

   procedure Initialize_Model
     (Model              : not null access Root_Carthage_Model'Class;
      House              : not null access constant
        Carthage.Houses.House_Record'Class;
      Class_Id           : String;
      Render_Layer_Count : Lui.Render_Layer);

   procedure Draw
     (Model     : Root_Carthage_Model'Class;
      Rectangle : Layout_Rectangle;
      Colour    : Lui.Colours.Colour_Type;
      Filled    : Boolean;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class);

   procedure Render_Main_Window
     (Model     : in out Root_Carthage_Model;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class;
      Layer     : Lui.Render_Layer;
      Rectangle : Layout_Rectangle)
   is abstract;

   procedure Set_Selected_Stack
     (Model : in out Root_Carthage_Model;
      Stack : not null access constant Carthage.Stacks.Stack_Record'Class);

   function Main_Rectangle
     (Model     : Root_Carthage_Model'Class)
      return Layout_Rectangle;

   function Minimap_Rectangle
     (Model     : Root_Carthage_Model'Class)
      return Layout_Rectangle;

   function House
     (Model : Root_Carthage_Model'Class)
      return Carthage.Houses.House_Type;

   function Wizard_Mode
     (Model : Root_Carthage_Model'Class)
      return Boolean;

   function Top_Model
     (House : not null access constant
        Carthage.Houses.House_Record'Class)
     return Lui.Models.Object_Model;

private

   type Resource_Layout_Record is
      record
         Rectangle : Layout_Rectangle;
      end record;

   type Resource_Layout_Array is
     array (Carthage.Resources.Resource_Index range <>)
     of Resource_Layout_Record;

   type Root_Carthage_Model is
     abstract new Lui.Models.Root_Object_Model with
      record
         House                 : Carthage.Houses.House_Type;
         Stack                 : Carthage.Stacks.Stack_Type;
         Wizard                : Boolean := False;
         Layout_Loaded         : Boolean := False;
         Left_Toolbar_Layout   : Layout_Rectangle;
         Top_Toolbar_Layout    : Layout_Rectangle;
         Bottom_Toolbar_Layout : Layout_Rectangle;
         Main_Rectangle        : Layout_Rectangle;
         Mini_Map_Layout       : Layout_Rectangle;
         Status_Layout         : Layout_Rectangle;
         Selected_Stack_Layout : Layout_Rectangle;
         Sidebar_Icon_Size     : Positive := 64;
         Resource_Layout       : access Resource_Layout_Array;
      end record;

   function House
     (Model : Root_Carthage_Model'Class)
      return Carthage.Houses.House_Type
   is (Model.House);

   function Wizard_Mode
     (Model : Root_Carthage_Model'Class)
      return Boolean
   is (Model.Wizard);

   function Main_Rectangle
     (Model     : Root_Carthage_Model'Class)
      return Layout_Rectangle
   is (Model.Main_Rectangle);

   function Minimap_Rectangle
     (Model     : Root_Carthage_Model'Class)
      return Layout_Rectangle
   is (Model.Mini_Map_Layout);

end Carthage.UI.Models.Top;
