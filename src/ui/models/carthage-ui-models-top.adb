with Tropos.Reader;

with Carthage.Calendar;

with Carthage.UI.Models.Galaxy;

with Carthage.Paths;

package body Carthage.UI.Models.Top is

   Layout_Config : Tropos.Configuration;
   Have_Layout   : Boolean := False;

   Static_UI_Layer      : constant Lui.Render_Layer := 1;
   Minimap_Layer        : constant Lui.Render_Layer := 2;
   UI_Layer             : constant Lui.Render_Layer := 3;
   Selected_Stack_Layer : constant Lui.Render_Layer := 4;
   Last_Top_Layer       : constant Lui.Render_Layer := Selected_Stack_Layer;

   procedure Load_Layout
     (Model : in out Root_Carthage_Model'Class);

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Model     : Root_Carthage_Model'Class;
      Rectangle : Layout_Rectangle;
      Colour    : Lui.Colours.Colour_Type;
      Filled    : Boolean;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class)
   is
      pragma Unreferenced (Model);
   begin
      Renderer.Draw_Rectangle
        (X      => Rectangle.X,
         Y      => Rectangle.Y,
         W      => Rectangle.Width,
         H      => Rectangle.Height,
         Colour => Colour,
         Filled => Filled);
   end Draw;

   ----------------------
   -- Initialize_Model --
   ----------------------

   procedure Initialize_Model
     (Model              : not null access Root_Carthage_Model'Class;
      House              : not null access constant
        Carthage.Houses.House_Record'Class;
      Class_Id           : String;
      Render_Layer_Count : Lui.Render_Layer)
   is
      use type Lui.Render_Layer;
   begin
      Model.House := Carthage.Houses.House_Type (House);
      Save_Model (Model, Class_Id);

      Model.Initialise
        (Name              => Class_Id,
         Last_Render_Layer => Last_Top_Layer + Render_Layer_Count);
      Model.Set_Background (Lui.Colours.Black);
   end Initialize_Model;

   -----------------
   -- Load_Layout --
   -----------------

   procedure Load_Layout
     (Model : in out Root_Carthage_Model'Class)
   is
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
     (Model    : in out Root_Carthage_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      use Lui;

      procedure Render_Static_UI;

      ----------------------
      -- Render_Static_UI --
      ----------------------

      procedure Render_Static_UI is
      begin
         Model.Draw
           (Rectangle => Model.Status_Layout,
            Colour => (0.2, 0.2, 0.2, 1.0),
            Filled => True,
            Renderer => Renderer);
         Renderer.Draw_String
           (Model.Status_Layout.X + 4,
            Model.Status_Layout.Y + 24,
            20, Lui.Colours.White,
            Carthage.Calendar.Image (Carthage.Calendar.Clock));
      end Render_Static_UI;

   begin
      if not Model.Layout_Loaded then
         Model.Load_Layout;
      end if;

      case Renderer.Current_Render_Layer is
         when Static_UI_Layer =>
            Render_Static_UI;
         when Minimap_Layer =>
            null;
         when UI_Layer =>
            null;
         when Selected_Stack_Layer =>
            null;
         when others =>
            Root_Carthage_Model'Class (Model)
              .Render_Main_Window
                (Renderer,
                 Renderer.Current_Render_Layer - Last_Top_Layer,
                 Model.Main_Rectangle);
      end case;
   end Render;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (Item          : in out Root_Carthage_Model;
      Width, Height : Natural)
   is
   begin
      Lui.Models.Root_Object_Model (Item).Resize (Width, Height);
      Item.Layout_Loaded := False;
   end Resize;

   ------------------------
   -- Set_Selected_Stack --
   ------------------------

   procedure Set_Selected_Stack
     (Model : in out Root_Carthage_Model;
      Stack : not null access constant Carthage.Stacks.Stack_Record'Class)
   is
   begin
      Model.Stack := Carthage.Stacks.Stack_Type (Stack);
   end Set_Selected_Stack;

   ---------------
   -- Top_Model --
   ---------------

   function Top_Model
     (House : not null access constant
        Carthage.Houses.House_Record'Class)
      return Lui.Models.Object_Model
   is
   begin
      if not Have_Layout then
         Layout_Config :=
           Tropos.Reader.Read_Config
             (Carthage.Paths.Config_File ("ui/layout.txt"));
         Have_Layout := True;
      end if;

      return Carthage.UI.Models.Galaxy.Galaxy_Model (House);
   end Top_Model;

end Carthage.UI.Models.Top;
