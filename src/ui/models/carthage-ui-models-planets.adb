with Ada.Containers.Doubly_Linked_Lists;

with Tropos.Reader;

with Lui.Rendering;

with Hexes;

with Carthage.Assets;
with Carthage.Cities;
with Carthage.Stacks;
with Carthage.Tiles;

with Carthage.UI.Maps;

with Carthage.Options;
with Carthage.Paths;

package body Carthage.UI.Models.Planets is

   subtype Zoom_Level is Integer range -3 .. 3;

   Zoomed_Tile_Size : constant array (Zoom_Level) of Positive :=
                        (160, 120, 80, 40, 30, 20, 10);

   Zoomed_Icon_Size : constant array (Zoom_Level) of Positive :=
                        (64, 48, 32, 32, 24, 16, 8);

   Base_Layer        : constant Lui.Rendering.Render_Layer := 1;
   Unit_Layer        : constant Lui.Rendering.Render_Layer := 2;
   Path_Layer        : constant Lui.Rendering.Render_Layer := 3;
   Selection_Layer   : constant Lui.Rendering.Render_Layer := 4;
   Minimap_Layer     : constant Lui.Rendering.Render_Layer := 5;
   UI_Layer          : constant Lui.Rendering.Render_Layer := 6;
   Last_Layer        : constant Lui.Rendering.Render_Layer := UI_Layer;

   Layout_Config : Tropos.Configuration;
   Have_Layout   : Boolean := False;

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

   procedure Draw
     (Rectangle : Layout_Rectangle;
      Colour    : Lui.Colours.Colour_Type;
      Filled    : Boolean;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class);

   subtype Planet_Model_Layer is
     Lui.Rendering.Render_Layer range 1 .. Last_Layer;

   type Rendered_Stack_Icon is
      record
         Stack : Carthage.Stacks.Stack_Type;
         Left, Top, Right, Bottom : Integer;
      end record;

   package Rendered_Stack_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rendered_Stack_Icon);

   type Rendered_Asset_Icon is
      record
         Asset : Carthage.Assets.Asset_Type;
         Rec   : Layout_Rectangle;
      end record;

   package Rendered_Asset_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rendered_Asset_Icon);

   type Root_Planet_Model is
     new Root_Carthage_Model with
      record
         Planet                : Carthage.Planets.Planet_Type;
         Centre                : Tile_Position;
         Selected_Stack        : Carthage.Stacks.Stack_Type;
         Rendered_Stacks       : Rendered_Stack_Lists.List;
         Rendered_Assets       : Rendered_Asset_Lists.List;
         Needs_Render          : Boolean := False;
         Layout_Loaded         : Boolean := False;
         Current_Zoom          : Zoom_Level := 0;
         Left_Toolbar_Layout   : Layout_Rectangle;
         Top_Toolbar_Layout    : Layout_Rectangle;
         Bottom_Toolbar_Layout : Layout_Rectangle;
         Main_Map_Layout       : Layout_Rectangle;
         Mini_Map_Layout       : Layout_Rectangle;
         Selected_Stack_Layout : Layout_Rectangle;
         Sidebar_Icon_Size     : Positive;
         Show_Hex_Coords       : Boolean;
         Show_Cubic_Coords     : Boolean;
         Show_Move_Cost        : Boolean;
      end record;

   overriding function Handle_Update
     (Model    : in out Root_Planet_Model)
      return Boolean
   is (Model.Needs_Render);

   overriding procedure Resize
     (Item          : in out Root_Planet_Model;
      Width, Height : Natural);

   overriding procedure Render
     (Model    : in out Root_Planet_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding procedure On_Model_Removed
     (Model : in out Root_Planet_Model;
      Child : not null access Lui.Models.Root_Object_Model'Class)
   is null;

   overriding procedure Select_XY
     (Model : in out Root_Planet_Model;
      X, Y  : Natural);

   overriding procedure Zoom
     (Model   : in out Root_Planet_Model;
      Z       : in     Integer;
      Control : in     Boolean);

   overriding function Tooltip
     (Model : Root_Planet_Model;
      X, Y  : Natural)
      return String;

   overriding procedure After_Transition
     (Model : in out Root_Planet_Model)
   is null;

   overriding procedure Reload
     (Model : in out Root_Planet_Model)
   is null;

   overriding procedure On_Drag
     (Model : in out Root_Planet_Model;
      DX, DY : Integer)
   is null;

   procedure Scan_Screen_Tiles
     (Model   : Root_Planet_Model'Class;
      Process : not null access
        procedure (Tile : Carthage.Tiles.Tile_Type;
                   Screen_X, Screen_Y : Integer));

   procedure Load_Layout
     (Model : in out Root_Planet_Model'Class);

   procedure Get_Screen_Tile_Centre
     (Model    : Root_Planet_Model'Class;
      Position : Tile_Position;
      X, Y     : out Integer);

   function Icon_Size
     (Model : Root_Planet_Model'Class)
      return Natural
   is (Zoomed_Icon_Size (Model.Current_Zoom));

   function Tile_Width
     (Model : Root_Planet_Model'Class)
      return Natural
   is (Zoomed_Tile_Size (Model.Current_Zoom) * 6 / 5)
   with Unreferenced;

   function Tile_Height
     (Model : Root_Planet_Model'Class)
      return Natural
   is (Zoomed_Tile_Size (Model.Current_Zoom));

   function Column_Width
     (Model : Root_Planet_Model'Class)
      return Natural
   is (Zoomed_Tile_Size (Model.Current_Zoom));

   function Row_Height
     (Model : Root_Planet_Model'Class)
      return Natural
   is (Zoomed_Tile_Size (Model.Current_Zoom));

   function Map_Pixel_Width
     (Model : Root_Planet_Model'Class)
      return Natural
   is (Model.Main_Map_Layout.Width);

   function Map_Pixel_Height
     (Model : Root_Planet_Model'Class)
      return Natural
   is (Model.Main_Map_Layout.Height);

   function Map_Hex_Left
     (Model : Root_Planet_Model'Class)
      return Tile_X;

   function Map_Hex_Top
     (Model : Root_Planet_Model'Class)
      return Tile_Y;

   function Map_Hex_Width
     (Model : Root_Planet_Model'Class)
      return Tile_X_Count;

   function Map_Hex_Height
     (Model : Root_Planet_Model'Class)
      return Tile_Y_Count;

--     function Map_Hex_Right
--       (Model : Root_Planet_Model'Class)
--        return Tile_X
--     is (Model.Map_Hex_Left + Model.Map_Hex_Width);
--
--     function Map_Hex_Bottom
--       (Model : Root_Planet_Model'Class)
--        return Tile_Y
--     is (Model.Map_Hex_Top + Model.Map_Hex_Height);
--

   procedure Set_Centre
     (Model : in out Root_Planet_Model'Class;
      Centre : Tile_Position);

   type Planet_Model_Type is access all Root_Planet_Model'Class;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Rectangle : Layout_Rectangle;
      Colour    : Lui.Colours.Colour_Type;
      Filled    : Boolean;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class)
   is
   begin
      Renderer.Draw_Rectangle
        (X      => Rectangle.X,
         Y      => Rectangle.Y,
         W      => Rectangle.Width,
         H      => Rectangle.Height,
         Colour => Colour,
         Filled => Filled);
   end Draw;

   ----------------------------
   -- Get_Screen_Tile_Centre --
   ----------------------------

   procedure Get_Screen_Tile_Centre
     (Model    : Root_Planet_Model'Class;
      Position : Tile_Position;
      X, Y     : out Integer)
   is
      Relative_Y : constant Integer :=
                     Integer (Position.Y) - Integer (Model.Centre.Y);
      Relative_X : constant Integer :=
                     Integer (Position.X) - Integer (Model.Centre.X);
      Screen_Y   : constant Integer :=
                     Model.Main_Map_Layout.Y
                       + Model.Height / 2
                       + Relative_Y * Model.Row_Height
                       + (if Position.X mod 2 = 1
                          then Model.Row_Height / 2 else 0);
      Screen_X   : constant Integer :=
                     Model.Main_Map_Layout.X
                     + Model.Map_Pixel_Width / 2
                     + (if abs Relative_X <= Planet_Width / 2
                        then Relative_X * Model.Column_Width
                        elsif Relative_X < 0
                        then (Relative_X + Planet_Width) * Model.Column_Width
                        else (Planet_Width - Relative_X) * Model.Column_Width);
   begin
      X := Screen_X;
      Y := Screen_Y;
   end Get_Screen_Tile_Centre;

   -----------------
   -- Load_Layout --
   -----------------

   procedure Load_Layout
     (Model : in out Root_Planet_Model'Class)
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
      Model.Main_Map_Layout := Rectangle ("main-map");
      Model.Selected_Stack_Layout := Rectangle ("selected-stack-layout");
      Model.Sidebar_Icon_Size := Layout_Config.Get ("sidebar-icon-size", 64);
      Model.Layout_Loaded := True;
   end Load_Layout;

   --------------------
   -- Map_Hex_Height --
   --------------------

   function Map_Hex_Height
     (Model : Root_Planet_Model'Class)
      return Tile_Y_Count
   is
      Available : constant Natural :=
                    Model.Map_Pixel_Height / Model.Row_Height;
   begin
      if Available >= Planet_Height then
         return Tile_Y_Count'Last;
      else
         return Tile_Y_Count (Available);
      end if;
   end Map_Hex_Height;

   ------------------
   -- Map_Hex_Left --
   ------------------

   function Map_Hex_Left
     (Model : Root_Planet_Model'Class)
      return Tile_X
   is
   begin
      if Model.Centre.X <= Model.Map_Hex_Width / 2 then
         return Tile_X'First;
      else
         return Model.Centre.X - Model.Map_Hex_Width / 2;
      end if;
   end Map_Hex_Left;

   -----------------
   -- Map_Hex_Top --
   -----------------

   function Map_Hex_Top
     (Model : Root_Planet_Model'Class)
      return Tile_Y
   is
   begin
      if Model.Centre.Y <= Model.Map_Hex_Height / 2 then
         return Tile_Y'First;
      else
         return Model.Centre.Y - Model.Map_Hex_Height / 2;
      end if;
   end Map_Hex_Top;

   -------------------
   -- Map_Hex_Width --
   -------------------

   function Map_Hex_Width
     (Model : Root_Planet_Model'Class)
      return Tile_X_Count
   is
      Available : constant Natural :=
                    Model.Map_Pixel_Width / Model.Column_Width;
   begin
      if Available >= Planet_Width then
         return Tile_X_Count'Last;
      else
         return Tile_X_Count (Available);
      end if;
   end Map_Hex_Width;

   ------------------
   -- Planet_Model --
   ------------------

   function Planet_Model
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Carthage_Model
   is
      use Carthage.Houses;
   begin
      if not Have_Layout then
         Layout_Config :=
           Tropos.Reader.Read_Config
             (Carthage.Paths.Config_File ("ui/layout.txt"));
         Have_Layout := True;
      end if;

      if not Have_Model (House, Planet.Identifier) then
         declare
            use Carthage.Planets;
            Model : constant Planet_Model_Type := new Root_Planet_Model;
         begin
            Model.Initialise (Planet.Name, Last_Render_Layer => Last_Layer);
            Model.Set_Background
              (Lui.Colours.To_Colour (100, 100, 100));
            Model.Planet := Planet;
            Set_Model (House, Planet.Identifier, Model);
            Model.Show_Hex_Coords := Carthage.Options.Show_Hex_Coordinates;
            Model.Show_Cubic_Coords :=
              Carthage.Options.Show_Cubic_Coordinates;
            Model.Show_Move_Cost :=
              Carthage.Options.Show_Move_Cost;

            if Model.Planet.Has_Owner
              and then Model.House = Model.Planet.Owner
            then
               Model.Set_Centre (Model.Planet.Palace.Tile.Position);
            else
               Model.Set_Centre ((Planet_Width / 2, Planet_Height / 2));
            end if;
         end;
      end if;
      return Get_Model (House, Planet.Identifier);
   end Planet_Model;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_Planet_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      use Carthage.Planets;
      use type Carthage.Stacks.Stack_Type;

      Icon_Size    : constant Natural :=
                       Zoomed_Icon_Size (Model.Current_Zoom);

      Zoomed_Size  : constant Natural :=
                       Zoomed_Tile_Size (Model.Current_Zoom);
      Tile_Width   : constant Natural := Zoomed_Size * 6 / 5;
      Tile_Height  : constant Natural := Zoomed_Size;

      procedure Draw_Base_Layer_Tile
        (Tile               : Carthage.Tiles.Tile_Type;
         Screen_X, Screen_Y : Integer);

      procedure Draw_Unit_Layer_Tile
        (Tile               : Carthage.Tiles.Tile_Type;
         Screen_X, Screen_Y : Integer);

      procedure Draw_Stack_Icons
        (Stack : Carthage.Stacks.Stack_Type);

      --------------------------
      -- Draw_Base_Layer_Tile --
      --------------------------

      procedure Draw_Base_Layer_Tile
        (Tile               : Carthage.Tiles.Tile_Type;
         Screen_X, Screen_Y : Integer)
      is
         use Carthage.UI.Maps;

         Layers   : Carthage.UI.Maps.Tile_Layers;

         procedure Draw
           (Element  : Layer_Element_Type;
            Resource : String;
            Color    : Carthage.Colours.Colour_Type);

         ----------
         -- Draw --
         ----------

         procedure Draw
           (Element  : Layer_Element_Type;
            Resource : String;
            Color    : Carthage.Colours.Colour_Type)
         is
            use type Carthage.Colours.Colour_Element;
         begin
            case Element is
               when Background_Hex_Tile =>
                  if not Renderer.Have_Resource (Resource) then
                     Renderer.Create_Image_Resource
                       (Resource_Name => Resource,
                        Image         =>
                          Create_Background_Hex
                            (Color));
                  end if;

                  Renderer.Draw_Image
                    (Screen_X - Tile_Width / 2,
                     Screen_Y - Tile_Height / 2,
                     Tile_Width, Tile_Height,
                     Resource);

               when Hex_Tile =>
                  pragma Assert
                    (Renderer.Have_Resource (Resource));

                  Renderer.Draw_Image
                    (Screen_X - Tile_Width / 2,
                     Screen_Y - Tile_Height / 2,
                     Tile_Width, Tile_Height,
                     Resource);
            end case;
         end Draw;

      begin
         Carthage.UI.Maps.Get_Tile_Layers
           (Model.Planet, Model.House, Tile.Position, Layers);

         Layers.Scan_Layers (Draw'Access);

         if Model.Show_Hex_Coords then
            Renderer.Draw_String
              (X      => Screen_X - Tile_Width / 2,
               Y      => Screen_Y,
               Size   => 8,
               Colour => Lui.Colours.Black,
               Text   => Carthage.Tiles.Position_Image (Tile.Position));
         end if;

         if Model.Show_Cubic_Coords then
            declare
               use Hexes;
               Cubic : constant Cube_Coordinate :=
                         Model.Planet.To_Cubic (Tile.Position);
            begin
               Renderer.Draw_String
                 (X      => Screen_X + Tile_Width / 4 - 6,
                  Y      => Screen_Y,
                  Size   => 12,
                  Colour => Lui.Colours.Black,
                  Text   => Coordinate_Type'Image (Cube_X (Cubic)));
               Renderer.Draw_String
                 (X      => Screen_X - Tile_Width / 4 - 6,
                  Y      => Screen_Y - Tile_Height / 2 + 4,
                  Size   => 12,
                  Colour => Lui.Colours.Black,
                  Text   => Coordinate_Type'Image (Cube_Y (Cubic)));
               Renderer.Draw_String
                 (X      => Screen_X - Tile_Width / 4 - 6,
                  Y      => Screen_Y + Tile_Height / 2 - 16,
                  Size   => 12,
                  Colour => Lui.Colours.Black,
                  Text   => Coordinate_Type'Image (Cube_Z (Cubic)));
            end;
         end if;

         if Model.Selected_Stack /= null
           and then Model.Show_Move_Cost
         then
            Renderer.Draw_String
              (X      => Screen_X,
               Y      => Screen_Y,
               Size   => 12,
               Colour => Lui.Colours.Black,
               Text   =>
                 Natural'Image
                   (Model.Selected_Stack.Movement_Cost (Tile)));
         end if;

      end Draw_Base_Layer_Tile;

      ----------------------
      -- Draw_Stack_Icons --
      ----------------------

      procedure Draw_Stack_Icons
        (Stack : Carthage.Stacks.Stack_Type)
      is
         X : Natural := Model.Selected_Stack_Layout.X;
         Y : Natural := Model.Selected_Stack_Layout.Y;
         Limit_X : constant Natural :=
                     X + Model.Selected_Stack_Layout.Width;
         Background : constant Carthage.Colours.Colour_Type :=
                        Stack.Owner.Colour;
         Icon_Size  : constant Positive := Model.Sidebar_Icon_Size;
      begin
         Model.Rendered_Assets.Clear;

         for I in 1 .. Stack.Count loop
            declare
               Asset      : constant Carthage.Assets.Asset_Type :=
                              Stack.Asset (I);
               Resource   : constant String :=
                              "unit"
                              & Integer'Image (-(Asset.Unit.Index));
            begin
               Renderer.Draw_Rectangle
                 (X, Y, Icon_Size, Icon_Size,
                  To_Lui_Colour (Background), True);
               Renderer.Draw_Image
                 (X, Y,
                  Icon_Size, Icon_Size,
                  Resource);
               Model.Rendered_Assets.Append
                 (Rendered_Asset_Icon'
                    (Asset => Asset,
                     Rec   => (X, Y, Icon_Size, Icon_Size)));
            end;

            X := X + Icon_Size + 2;
            if X + Icon_Size > Limit_X then
               X := Model.Selected_Stack_Layout.X;
               Y := Y + Icon_Size + 2;
            end if;
         end loop;
      end Draw_Stack_Icons;

      --------------------------
      -- Draw_Unit_Layer_Tile --
      --------------------------

      procedure Draw_Unit_Layer_Tile
        (Tile               : Carthage.Tiles.Tile_Type;
         Screen_X, Screen_Y : Integer)
      is
      begin
         if (Wizard_Mode or else Tile.Currently_Visible_To (Model.House))
           and then Tile.Has_Stacks
         then
            declare
               Left       : Integer :=
                              Screen_X - Icon_Size / 2;
               Right      : constant Integer := Left + Icon_Size;
               Top        : Integer :=
                              Screen_Y + Tile_Height / 2
                                - Icon_Size;
               Bottom     : constant Integer := Top + Icon_Size;
               Stack      : constant Carthage.Stacks.Stack_Type :=
                              Tile.First_Stack;
               Background : Carthage.Colours.Colour_Type :=
                              Stack.Owner.Colour;
               Resource   : constant String :=
                              "unit"
                              & Integer'Image (-(Stack.Asset (1).Unit.Index));
            begin
               if Stack.Has_Movement then
                  declare
                     Next_Position  : constant Tile_Position :=
                                        Stack.Next_Tile;
                     Progress       : constant Float :=
                                        Stack.Movement_Progress;
                     Next_X, Next_Y : Integer;
                  begin
                     Model.Get_Screen_Tile_Centre
                       (Next_Position, Next_X, Next_Y);
                     Left := Left + Integer (Float (Next_X - Left) * Progress);
                     Top := Top + Integer (Float (Next_Y - Top) * Progress);
                  end;
               end if;

               Model.Rendered_Stacks.Append
                 (Rendered_Stack_Icon'
                    (Stack, Left, Top, Right, Bottom));

               Background.Alpha := 0.7;

               Renderer.Draw_Rectangle
                 (Left, Top, Icon_Size, Icon_Size,
                  To_Lui_Colour (Background), True);
               Renderer.Draw_Image
                 (Left, Top, Icon_Size, Icon_Size, Resource);
            end;
         end if;
      end Draw_Unit_Layer_Tile;

   begin

      if not Model.Layout_Loaded then
         Model.Load_Layout;
      end if;

      case Planet_Model_Layer (Renderer.Current_Render_Layer) is
         when Base_Layer =>

            Model.Scan_Screen_Tiles (Draw_Base_Layer_Tile'Access);

         when Unit_Layer =>

            Model.Rendered_Stacks.Clear;
            Model.Scan_Screen_Tiles (Draw_Unit_Layer_Tile'Access);

         when Path_Layer =>

            if Model.Selected_Stack /= null
              and then Model.Selected_Stack.Has_Movement
            then
               declare
                  Path       : constant Array_Of_Positions :=
                                 Model.Selected_Stack.Current_Movement;
                  X1, Y1     : Integer := 0;
                  Past       : Boolean := True;
                  Past_Color : constant Lui.Colours.Colour_Type :=
                                 Lui.Colours.To_Colour (100, 100, 100);
                  Future_Color : constant Lui.Colours.Colour_Type :=
                                   Lui.Colours.To_Colour (150, 150, 0);
               begin
                  for Next of Path loop
                     declare
                        X2, Y2 : Integer;
                     begin
                        Model.Get_Screen_Tile_Centre (Next, X2, Y2);
                        if X1 /= 0 then
                           Renderer.Draw_Line
                             (X1, Y1, X2, Y2,
                              Colour     =>
                                (if Past then Past_Color else Future_Color),
                              Line_Width => 5);
                        end if;

                        if Next = Model.Selected_Stack.Tile.Position then
                              Past := False;
                        end if;

                        X1 := X2;
                        Y1 := Y2;
                     end;
                  end loop;
               end;
            end if;

         when Selection_Layer =>

            declare
               Screen_X, Screen_Y : Integer;
            begin
               if Model.Selected_Stack /= null then
                  Model.Get_Screen_Tile_Centre
                    (Model.Selected_Stack.Tile.Position,
                     Screen_X, Screen_Y);

                  Renderer.Draw_Rectangle
                    (X      =>
                       Screen_X - Model.Icon_Size / 2,
                     Y      =>
                       Screen_Y + Model.Tile_Height / 2 - Model.Icon_Size,
                     W      => Model.Icon_Size,
                     H      => Model.Icon_Size,
                     Colour =>
                       Lui.Colours.To_Colour
                         (0, 200, 0),
                     Filled => False);
               end if;
            end;
         when Minimap_Layer =>
            declare
               Scale : constant Positive :=
                         Integer'Max (Model.Left_Toolbar_Layout.Width
                                      / Planet_Width,
                                      1);

               procedure Draw_Minimap_Tile
                 (Tile               : Carthage.Tiles.Tile_Type);

               -----------------------
               -- Draw_Minimap_Tile --
               -----------------------

               procedure Draw_Minimap_Tile
                 (Tile               : Carthage.Tiles.Tile_Type)
               is
                  X     : constant Natural :=
                            Model.Mini_Map_Layout.X
                            + Natural (Tile.Position.X) * Scale;
                  Y     : constant Natural :=
                            Model.Mini_Map_Layout.Y
                              + Natural (Tile.Position.Y) * Scale;
                  Color : constant Carthage.Colours.Colour_Type :=
                            Tile.Base_Terrain.Colour
                              (Model.Planet.Category_Name);
               begin
                  Renderer.Draw_Rectangle
                    (X      => X,
                     Y      => Y,
                     W      => Scale,
                     H      => Scale,
                     Colour => To_Lui_Colour (Color),
                     Filled => True);
               end Draw_Minimap_Tile;

            begin
               Draw (Model.Left_Toolbar_Layout, Lui.Colours.Black, True,
                     Renderer);

               Model.Planet.Scan_Tiles (Draw_Minimap_Tile'Access);

               Renderer.Draw_Rectangle
                 (X      =>
                    Model.Mini_Map_Layout.X
                      + Natural (Model.Map_Hex_Left) * Scale,
                  Y      =>
                    Model.Mini_Map_Layout.Y
                      + Natural (Model.Map_Hex_Top) * Scale,
                  W      => Natural (Model.Map_Hex_Width) * Scale,
                  H      => Natural (Model.Map_Hex_Height) * Scale,
                  Colour => Lui.Colours.White,
                  Filled => False);

               if Model.Selected_Stack /= null then
                  Draw_Stack_Icons (Model.Selected_Stack);
               end if;

            end;

         when UI_Layer =>
            null;
      end case;
      Model.Needs_Render := False;
   end Render;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (Item          : in out Root_Planet_Model;
      Width, Height : Natural)
   is
   begin
      Root_Carthage_Model (Item).Resize (Width, Height);
      Item.Load_Layout;
   end Resize;

   -----------------------
   -- Scan_Screen_Tiles --
   -----------------------

   procedure Scan_Screen_Tiles
     (Model   : Root_Planet_Model'Class;
      Process : not null access
        procedure (Tile : Carthage.Tiles.Tile_Type;
                   Screen_X, Screen_Y : Integer))
   is
      Zoomed_Size  : constant Natural :=
                       Zoomed_Tile_Size (Model.Current_Zoom);
      Tile_Height  : constant Natural := Zoomed_Size;
      Column_Width : constant Natural := Zoomed_Size;

      Tiles_Across : constant Positive :=
                       Model.Map_Pixel_Width / Column_Width + 1;
      Tiles_Down   : constant Positive :=
                       Model.Map_Pixel_Height / Tile_Height + 1;

      Left         : constant Integer :=
                       Integer'Max
                         (Integer (Model.Centre.X) - Tiles_Across / 2,
                          1 - Planet_Width);
      Right        : constant Integer :=
                       Integer'Min
                         (Left + Tiles_Across - 1, 2 * Planet_Width);
      Top          : constant Integer :=
                       Integer'Max
                         (Integer (Model.Centre.Y) - Tiles_Down / 2,
                          1 - Planet_Height);
      Bottom       : constant Integer :=
                       Integer'Min
                         (Top + Tiles_Down - 1, 2 * Planet_Height);

      procedure Draw (Tile : Carthage.Tiles.Tile_Type);

      ----------
      -- Draw --
      ----------

      procedure Draw (Tile : Carthage.Tiles.Tile_Type) is
         Position : constant Tile_Position := Tile.Position;
         Screen_X : Integer;
         Screen_Y : Integer;
      begin
         Model.Get_Screen_Tile_Centre (Position, Screen_X, Screen_Y);
         Process (Tile, Screen_X, Screen_Y);
      end Draw;

   begin

      if False then
         Model.Planet.Scan_Tiles (Draw'Access);
      end if;

      for Y in Top .. Bottom loop
         if Y in 1 .. Planet_Height then
            for Extended_X in Left .. Right loop
               declare
                  X : constant Tile_X :=
                        Tile_X
                          (if Extended_X < 1
                           then Extended_X + Planet_Width
                           elsif Extended_X > Planet_Width
                           then Extended_X - Planet_Width
                           else Extended_X);

                  Position : constant Tile_Position :=
                               (X, Tile_Y (Y));
                  Tile     : constant Carthage.Tiles.Tile_Type :=
                               Model.Planet.Tile (Position);
                  Screen_X : Integer;
                  Screen_Y : Integer;

               begin
                  Model.Get_Screen_Tile_Centre (Position, Screen_X, Screen_Y);

                  if Extended_X < 1 then
                     Screen_X := Screen_X - Column_Width * Planet_Width;
                  elsif Extended_X > Planet_Width then
                     Screen_X := Screen_X + Column_Width * Planet_Width;
                  end if;

                  Process (Tile, Screen_X, Screen_Y);

               end;
            end loop;
         end if;
      end loop;
   end Scan_Screen_Tiles;

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Model : in out Root_Planet_Model;
      X, Y  : Natural)
   is
   begin
      if Contains (Model.Main_Map_Layout, X, Y) then
         for Rendered_Stack of Model.Rendered_Stacks loop
            if X in Rendered_Stack.Left .. Rendered_Stack.Right
              and then Y in Rendered_Stack.Top .. Rendered_Stack.Bottom
            then
               Model.Selected_Stack := Rendered_Stack.Stack;
               Model.Queue_Render;
               exit;
            end if;
         end loop;
      elsif Contains (Model.Mini_Map_Layout, X, Y) then
         declare
            Zoomed_Size  : constant Natural :=
                             Zoomed_Tile_Size (Model.Current_Zoom);
            Tile_Height  : constant Natural := Zoomed_Size;
            Column_Width : constant Natural := Zoomed_Size;

            Tiles_Across : constant Positive :=
                             Model.Map_Pixel_Width / Column_Width + 1;
            Tiles_Down   : constant Positive :=
                             Model.Map_Pixel_Height / Tile_Height + 1;
            Scale        : constant Positive :=
                         Integer'Max (Model.Left_Toolbar_Layout.Width
                                      / Planet_Width,
                                      1);
            Centre_X : Integer := (X - Model.Mini_Map_Layout.X) / Scale + 1;
            Centre_Y : Integer := (Y - Model.Mini_Map_Layout.Y) / Scale + 1;
         begin
            Centre_X := Integer'Max (Centre_X, Tiles_Across / 2);
            Centre_Y := Integer'Max (Centre_Y, Tiles_Down / 2);
            Centre_X := Integer'Min
              (Centre_X, Planet_Width - Tiles_Across / 2 + 1);
            Centre_Y := Integer'Min
              (Centre_Y, Planet_Height - Tiles_Down / 2 + 1);
            Centre_X := Integer'Max (Centre_X, 1);
            Centre_Y := Integer'Max (Centre_Y, 1);
            Centre_X := Integer'Min (Centre_X, Planet_Width);
            Centre_Y := Integer'Min (Centre_Y, Planet_Height);
            Model.Set_Centre ((Tile_X (Centre_X), Tile_Y (Centre_Y)));
            Model.Queue_Render;
         end;
      end if;
   end Select_XY;

   ----------------
   -- Set_Centre --
   ----------------

   procedure Set_Centre
     (Model  : in out Root_Planet_Model'Class;
      Centre : Tile_Position)
   is
   begin
      Model.Centre := Centre;
   end Set_Centre;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Model : Root_Planet_Model;
      X, Y  : Natural)
      return String
   is
   begin
      if Contains (Model.Main_Map_Layout, X, Y) then
         declare
            Zoomed_Size  : constant Natural :=
                             Zoomed_Tile_Size (Model.Current_Zoom);
            Tile_Height  : constant Natural := Zoomed_Size;
            Column_Width : constant Natural := Zoomed_Size;
            Row_Height   : constant Natural := Tile_Height + 1;

            Map_X        : Integer :=
                             Integer (Model.Centre.X)
                             + (X - Model.Width / 2) / Column_Width;
            Map_Y        : constant Integer :=
                             Integer (Model.Centre.Y)
                             + (Y - Model.Height / 2) / Row_Height;
         begin
            while Map_X < 1 loop
               Map_X := Map_X + Planet_Width;
            end loop;
            while Map_X > Planet_Width loop
               Map_X := Map_X - Planet_Width;
            end loop;
            if Map_Y in 1 .. Planet_Height then
               return X'Img & Y'Img & " "
                 & Model.Planet.Tile ((Tile_X (Map_X), Tile_Y (Map_Y)))
                 .Description;
            else
               return "";
            end if;
         end;
      elsif Contains (Model.Selected_Stack_Layout, X, Y) then
         for Info of Model.Rendered_Assets loop
            if Contains (Info.Rec, X, Y) then
               return Info.Asset.Unit.Name;
            end if;
         end loop;
         return "";
      else
         return "";
      end if;

   end Tooltip;

   ----------
   -- Zoom --
   ----------

   overriding procedure Zoom
     (Model   : in out Root_Planet_Model;
      Z       : in     Integer;
      Control : in     Boolean)
   is
      pragma Unreferenced (Control);
      New_Zoom : constant Integer := Integer (Model.Current_Zoom) + Z;
   begin
      if New_Zoom in Zoom_Level then
         Model.Current_Zoom := New_Zoom;
         Model.Needs_Render := True;
      end if;
   end Zoom;

end Carthage.UI.Models.Planets;
