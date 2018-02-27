with Ada.Text_IO;

with Tropos.Reader;

with Carthage.Calendar;

with Carthage.Paths;

package body Carthage.UI.Models.Top is

   Class_Id : constant String := "top";

   Layout_Config : Tropos.Configuration;
   Have_Layout   : Boolean := False;

   UI_Layer          : constant Lui.Render_Layer := 1;
   Last_Render_Layer : constant Lui.Render_Layer := UI_Layer;

   subtype Top_Model_Render_Layer is
     Lui.Render_Layer range 1 .. Last_Render_Layer;

   procedure Load_Layout
     (Model : in out Top_Carthage_Model'Class);

   procedure Draw
     (Model     : Top_Carthage_Model'Class;
      Rectangle : Lui.Layout_Rectangle;
      Color     : Lui.Colors.Color_Type;
      Filled    : Boolean;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class);

   ----------------------
   -- Create_Top_Model --
   ----------------------

   function Create_Top_Model
     (House : Carthage.Houses.House_Type)
      return Top_Model
   is
   begin
      if not Have_Layout then
         Layout_Config :=
           Tropos.Reader.Read_Config
             (Carthage.Paths.Config_File ("ui/layout.txt"));
         Have_Layout := True;
      end if;

      declare
         Top : constant Top_Model := new Top_Carthage_Model;
      begin
         Top.Initialize_Model (House);
         return Top;
      end;
   end Create_Top_Model;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Model     : Top_Carthage_Model'Class;
      Rectangle : Lui.Layout_Rectangle;
      Color     : Lui.Colors.Color_Type;
      Filled    : Boolean;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class)
   is
      pragma Unreferenced (Model);
   begin
      Renderer.Set_Color (Color);
      Renderer.Rectangle (Rectangle, Filled);
   end Draw;

   ----------------------
   -- Initialize_Model --
   ----------------------

   procedure Initialize_Model
     (Model : not null access Top_Carthage_Model'Class;
      House : not null access constant Carthage.Houses.House_Record'Class)
   is
      use type Lui.Render_Layer;
   begin
      Model.House := Carthage.Houses.House_Type (House);
      Model.Initialise
        (Name              => Class_Id,
         Last_Render_Layer => Last_Render_Layer);
      Model.Set_Background (Lui.Colors.Black);
   end Initialize_Model;

   -----------------
   -- Load_Layout --
   -----------------

   procedure Load_Layout
     (Model : in out Top_Carthage_Model'Class)
   is
      use Lui;

      function Rectangle (Name : String) return Layout_Rectangle;

      ---------------
      -- Rectangle --
      ---------------

      function Rectangle (Name : String) return Layout_Rectangle is
         Config : constant Tropos.Configuration :=
                    Layout_Config.Child (Name);
         Left   : constant Integer := Config.Get (1);
         Top    : constant Integer := Config.Get (2);
         Right  : constant Integer := Config.Get (3);
         Bottom : constant Integer := Config.Get (4);
         X1     : constant Integer :=
                    (if Left < 0 then Model.Width + Left else Left);
         Y1     : constant Integer :=
                    (if Top < 0 then Model.Height + Top else Top);
         X2     : constant Integer :=
                    (if Left < 0 or else Right < 0
                     then Model.Width - abs Right else Right);
         Y2     : constant Integer :=
                    (if Top < 0 or else Bottom < 0
                     then Model.Height - abs Bottom else Bottom);
      begin
         return Layout_Rectangle'
           (X      => X1,
            Y      => Y1,
            Width  => Integer'Max (X2 - X1, 1),
            Height => Integer'Max (Y2 - Y1, 1));
      end Rectangle;

   begin
      Model.Left_Toolbar_Layout := Rectangle ("left-toolbar");
      Model.Top_Toolbar_Layout := Rectangle ("top-toolbar");
      Model.Bottom_Toolbar_Layout := Rectangle ("bottom-toolbar");
      Model.Mini_Map_Layout := Rectangle ("mini-map");
      Model.Main_Rectangle := Rectangle ("main-map");
      Model.Status_Layout := Rectangle ("status-box");
      Model.Selected_Stack_Layout := Rectangle ("selected-stack-layout");
      Model.Sidebar_Icon_Size := Layout_Config.Get ("sidebar-icon-size", 64);

      declare
         use Carthage.Resources;
         Next_Index : Resource_Index := 1;
         Current_X  : Integer := Model.Bottom_Toolbar_Layout.X + 2;
         Current_Y  : constant Integer := Model.Bottom_Toolbar_Layout.Y + 2;

         procedure Next_Rectangle
           (Resource : Carthage.Resources.Resource_Type);

         --------------------
         -- Next_Rectangle --
         --------------------

         procedure Next_Rectangle
           (Resource : Carthage.Resources.Resource_Type)
         is
         begin
            Model.Resource_Layout (Resource.Index) :=
              (Rectangle => (Current_X, Current_Y, 36, 48));
            Next_Index := Next_Index + 1;
            Current_X := Current_X + 40;
         end Next_Rectangle;

      begin

         if Model.Resource_Layout = null then
            Model.Resource_Layout :=
              new Resource_Layout_Array
                (1 .. Carthage.Resources.Last_Index);
         end if;

         Carthage.Resources.Scan (Next_Rectangle'Access);
      end;

      Model.Layout_Loaded := True;
   end Load_Layout;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Top_Carthage_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      Layer    : Lui.Render_Layer)
   is
      use Lui;

      procedure Render_Static_UI;

      ----------------------
      -- Render_Static_UI --
      ----------------------

      procedure Render_Static_UI is
      begin
         Renderer.Image
           (Rec      => (0, 0, Model.Width, Model.Height),
            Resource => "ui-background");

         Model.Draw
           (Rectangle => Model.Status_Layout,
            Color => (0.2, 0.2, 0.2, 1.0),
            Filled => True,
            Renderer => Renderer);

         Renderer.Set_Color (Lui.Colors.White);
         Renderer.Set_Font ("Tahoma", 20.0);

         Renderer.Text
           (Model.Status_Layout.X + 4,
            Model.Status_Layout.Y + 24,
            Carthage.Calendar.Image (Carthage.Calendar.Clock));

      end Render_Static_UI;

   begin
      if not Model.Layout_Loaded then
         Model.Load_Layout;
      end if;

      Ada.Text_IO.Put_Line
        ("top model render"
         & Natural'Image (Model.Width)
         & Natural'Image (Model.Height));

      case Top_Model_Render_Layer (Layer) is
         when UI_Layer =>
            Render_Static_UI;
      end case;

   end Render;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (Item          : in out Top_Carthage_Model;
      Width, Height : Natural)
   is
   begin
      Lui.Models.Root_Object_Model (Item).Resize (Width, Height);
      Item.Layout_Loaded := False;
   end Resize;

   ------------------------
   -- Set_Selected_Stack --
   ------------------------

--     procedure Set_Selected_Stack
--       (Model : in out Top_Carthage_Model;
--        Stack : not null access constant Carthage.Stacks.Stack_Record'Class)
--     is
--     begin
--        Model.Stack := Carthage.Stacks.Stack_Type (Stack);
--     end Set_Selected_Stack;

end Carthage.UI.Models.Top;
