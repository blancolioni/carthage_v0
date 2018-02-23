private with Ada.Containers.Doubly_Linked_Lists;

with Lui.Rendering;

with Carthage.Assets;
with Carthage.Stacks;

package Carthage.UI.Models.Stacks is

   type Rendered_Stack_List is tagged private;

   function Is_Empty (List : Rendered_Stack_List'Class) return Boolean;

   procedure Clear (List : in out Rendered_Stack_List'Class);

   procedure Add_Stack
     (List          : in out Rendered_Stack_List'Class;
      Stack         : Carthage.Stacks.Stack_Type;
      Left, Top     : Integer;
      Width, Height : Natural);

   procedure Render
     (List     : Rendered_Stack_List'Class;
      Model    : Root_Carthage_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   function Find_Stack
     (List : Rendered_Stack_List'Class;
      X, Y : Integer)
      return Carthage.Stacks.Stack_Type;

   function Asset_Resource
     (Asset : Carthage.Assets.Asset_Type)
      return String;

private

   type Rendered_Stack_Record is
      record
         Stack         : Carthage.Stacks.Stack_Type;
         Left, Top     : Integer;
         Width, Height : Natural;
      end record;

   package Rendered_Stack_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rendered_Stack_Record);

   type Rendered_Stack_List is new Rendered_Stack_Lists.List with null record;

   function Is_Empty (List : Rendered_Stack_List'Class) return Boolean
   is (Rendered_Stack_Lists.List (List).Is_Empty);

   function Asset_Resource
     (Asset : Carthage.Assets.Asset_Type)
      return String
   is ("unit"
       & Integer'Image (-(Asset.Unit.Index)));

end Carthage.UI.Models.Stacks;
