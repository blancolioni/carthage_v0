with Ada.Characters.Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;

with Lui.Rendering;

with Hexes;

with Carthage.Assets;
with Carthage.Cities;
with Carthage.Resources;
with Carthage.Stacks;
with Carthage.Tiles;

with Carthage.UI.Maps;
with Carthage.UI.Models.Top;

with Carthage.Options;

package body Carthage.UI.Models.Planets is

   subtype Zoom_Level is Integer range -3 .. 3;

   Zoomed_Tile_Size : constant array (Zoom_Level) of Positive :=
                        (160, 120, 80, 40, 30, 20, 10);

   Zoomed_Icon_Size : constant array (Zoom_Level) of Positive :=
                        (64, 48, 32, 32, 24, 16, 8);

   Base_Layer        : constant Lui.Render_Layer := 1;
   Unit_Layer        : constant Lui.Render_Layer := 2;
   Path_Layer        : constant Lui.Render_Layer := 3;
   Selection_Layer   : constant Lui.Render_Layer := 4;
   Last_Layer        : constant Lui.Render_Layer := Selection_Layer;

--     Resource_Image_Width  : constant := 34;
--     Resource_Image_Height : constant := 29;

   subtype Planet_Model_Layer is
     Lui.Render_Layer range 1 .. Last_Layer;

   type Rendered_Stack_Icon is
      record
         Stack : Carthage.Stacks.Stack_Type;
         Left, Top, Right, Bottom : Integer;
      end record;

   package Rendered_Stack_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rendered_Stack_Icon);

   type Hex_Layout_Record is
      record
         Rec  : Lui.Layout_Rectangle;
         Tile : Carthage.Tiles.Tile_Type;
      end record;

   package Hex_Layout_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Hex_Layout_Record);

   type Root_Planet_Model is
     new Carthage.UI.Models.Root_Carthage_Model with
      record
         Planet                : Carthage.Planets.Planet_Type;
         Centre                : Tile_Position;
         Selected_Stack        : Carthage.Stacks.Stack_Type;
         Rendered_Stacks       : Rendered_Stack_Lists.List;
         Current_Zoom          : Zoom_Level := 0;
         Hex_Layout            : Hex_Layout_Lists.List;
         Show_Hex_Coords       : Boolean;
         Show_Cubic_Coords     : Boolean;
         Hex_Layout_Changed    : Boolean := True;
      end record;

   overriding procedure Resize
     (Item          : in out Root_Planet_Model);

   overriding procedure Update
     (Item          : in out Root_Planet_Model);

   overriding procedure Render
     (Model    : in out Root_Planet_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      Layer     : Lui.Render_Layer);

   overriding procedure Select_XY
     (Model : not null access Root_Planet_Model;
      X, Y  : Natural);

   overriding procedure On_Key_Press
     (Model : in out Root_Planet_Model;
      Key   : Character);

   overriding procedure Zoom
     (Model   : in out Root_Planet_Model;
      Z       : in     Integer;
      X, Y    : in     Integer;
      Control : in     Boolean);

   overriding function Tooltip
     (Model : Root_Planet_Model;
      X, Y  : Natural)
      return String;

   overriding procedure After_Transition
     (Model : in out Root_Planet_Model)
   is null;

   overriding procedure On_Drag
     (Model : in out Root_Planet_Model;
      DX, DY : Integer)
   is null;

   function Get_Tile
     (Model : Root_Planet_Model'Class;
      X, Y  : Integer)
      return Carthage.Tiles.Tile_Type;

   procedure Scan_Screen_Tiles
     (Model   : Root_Planet_Model'Class;
      Process : not null access
        procedure (Tile : Carthage.Tiles.Tile_Type;
                   Screen_X, Screen_Y : Integer));

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

   function Map_Hex_Width
     (Model : Root_Planet_Model'Class)
      return Tile_X_Count
     with Unreferenced;

   function Map_Hex_Height
     (Model : Root_Planet_Model'Class)
      return Tile_Y_Count
     with Unreferenced;

   procedure Set_Centre
     (Model : in out Root_Planet_Model'Class;
      Centre : Tile_Position);

   type Planet_Model_Type is access all Root_Planet_Model'Class;

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
                     Model.Height / 2
                       + Relative_Y * Model.Row_Height
                     + (if Position.X mod 2 = 1
                        then Model.Row_Height / 2 else 0);
      Screen_X   : constant Integer :=
                     Model.Width / 2
                     + (if abs Relative_X <= Planet_Width / 2
                        then Relative_X * Model.Column_Width
                        elsif Relative_X < 0
                        then (Relative_X + Planet_Width) * Model.Column_Width
                        else (Planet_Width - Relative_X) * Model.Column_Width);
   begin
      X := Screen_X;
      Y := Screen_Y;
   end Get_Screen_Tile_Centre;

   --------------
   -- Get_Tile --
   --------------

   function Get_Tile
     (Model : Root_Planet_Model'Class;
      X, Y  : Integer)
      return Carthage.Tiles.Tile_Type
   is
      use Carthage.Tiles;
      Best_Tile : Tile_Type := null;
      Nearest   : Natural := 0;
   begin
      for Hex of Model.Hex_Layout loop
         if Lui.Contains (Hex.Rec, X, Y) then
            declare
               CX : constant Integer := Hex.Rec.X + Hex.Rec.Width / 2;
               CY : constant Integer := Hex.Rec.Y + Hex.Rec.Height / 2;
               D  : constant Natural := (X - CX) ** 2 + (Y - CY) ** 2;
            begin
               if Best_Tile = null or else D < Nearest then
                  Best_Tile := Hex.Tile;
                  Nearest := D;
               end if;
            end;
         end if;
      end loop;
      return Best_Tile;
   end Get_Tile;

   --------------------
   -- Map_Hex_Height --
   --------------------

   function Map_Hex_Height
     (Model : Root_Planet_Model'Class)
      return Tile_Y_Count
   is
      Available : constant Natural :=
                    Model.Height / Model.Row_Height;
   begin
      if Available >= Planet_Height then
         return Tile_Y_Count'Last;
      else
         return Tile_Y_Count (Available);
      end if;
   end Map_Hex_Height;

   -------------------
   -- Map_Hex_Width --
   -------------------

   function Map_Hex_Width
     (Model : Root_Planet_Model'Class)
      return Tile_X_Count
   is
      Available : constant Natural :=
                    Model.Width / Model.Column_Width;
   begin
      if Available >= Planet_Width then
         return Tile_X_Count'Last;
      else
         return Tile_X_Count (Available);
      end if;
   end Map_Hex_Width;

   ------------------
   -- On_Key_Press --
   ------------------

   overriding procedure On_Key_Press
     (Model : in out Root_Planet_Model;
      Key   : Character)
   is
   begin
      if Key = Ada.Characters.Latin_1.ESC then
         Carthage.UI.Models.Top.Top_Model
           (Model.Parent_Model)
           .Show_Galaxy;
      end if;
   end On_Key_Press;

   ------------------
   -- Planet_Model --
   ------------------

   function Planet_Model
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Carthage_Model
   is
      use Carthage.Houses;
      use Carthage.Planets;
      Model : constant Planet_Model_Type := new Root_Planet_Model;
   begin
      Model.Initialise (Planet.Identifier, Last_Layer);
      Model.House := House;
      Model.Planet := Planet;
      Save_Model (Model, Planet.Identifier);

      Model.Show_Hex_Coords := Carthage.Options.Show_Hex_Coordinates;
      Model.Show_Cubic_Coords :=
        Carthage.Options.Show_Cubic_Coordinates;

      if Model.Planet.Has_Owner
        and then Model.House = Model.Planet.Owner
      then
         Model.Set_Centre (Model.Planet.Palace.Tile.Position);
      else
         Model.Set_Centre ((Planet_Width / 2, Planet_Height / 2));
      end if;
      return Carthage_Model (Model);
   end Planet_Model;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_Planet_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      Layer     : Lui.Render_Layer)
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

--        procedure Draw_Stack_Icons
--          (Stack : Carthage.Stacks.Stack_Type);

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
            Color    : Carthage.Colors.Color_Type);

         ----------
         -- Draw --
         ----------

         procedure Draw
           (Element  : Layer_Element_Type;
            Resource : String;
            Color    : Carthage.Colors.Color_Type)
         is
            use type Carthage.Colors.Color_Element;
            Rec : constant Lui.Layout_Rectangle :=
                    Lui.Layout_Rectangle'
                      (X      => Screen_X - Tile_Width / 2,
                       Y      => Screen_Y - Tile_Width / 2,
                       Width  => Tile_Width,
                       Height => Tile_Height);
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

                  Renderer.Image (Rec, Resource);

                  if Model.Hex_Layout_Changed then
                     Model.Hex_Layout.Append
                       (Hex_Layout_Record'
                          (Rec  => Rec,
                           Tile => Tile));
                  end if;

               when Hex_Tile =>
                  pragma Assert
                    (Renderer.Have_Resource (Resource));

                  Renderer.Image (Rec, Resource);
            end case;
         end Draw;

      begin

         Carthage.UI.Maps.Get_Tile_Layers
           (Model.Planet, Model.House, Tile.Position, Layers);

         Layers.Scan_Layers (Draw'Access);

         if Model.Show_Hex_Coords then
            Renderer.Text
              (X     => Screen_X - Tile_Width / 2,
               Y     => Screen_Y,
               Value => Carthage.Tiles.Position_Image (Tile.Position));
         end if;

         if Model.Show_Cubic_Coords then
            declare
               use Hexes;
               Cubic : constant Cube_Coordinate :=
                         Model.Planet.To_Cubic (Tile.Position);
            begin
               Renderer.Text
                 (X      => Screen_X + Tile_Width / 4 - 6,
                  Y      => Screen_Y,
                  Value  => Coordinate_Type'Image (Cube_X (Cubic)));
               Renderer.Text
                 (X      => Screen_X - Tile_Width / 4 - 6,
                  Y      => Screen_Y - Tile_Height / 2 + 4,
                  Value  => Coordinate_Type'Image (Cube_Y (Cubic)));
               Renderer.Text
                 (X      => Screen_X - Tile_Width / 4 - 6,
                  Y      => Screen_Y + Tile_Height / 2 - 16,
                  Value  => Coordinate_Type'Image (Cube_Z (Cubic)));
            end;
         end if;

      end Draw_Base_Layer_Tile;

      ----------------------
      -- Draw_Stack_Icons --
      ----------------------

--        procedure Draw_Stack_Icons
--          (Stack : Carthage.Stacks.Stack_Type)
--        is
--           X : Natural := Model.Selected_Stack_Layout.X;
--           Y : Natural := Model.Selected_Stack_Layout.Y;
--           Limit_X : constant Natural :=
--                       X + Model.Selected_Stack_Layout.Width;
--           Background : constant Carthage.Colors.Color_Type :=
--                          Stack.Owner.Color;
--           Icon_Size  : constant Positive := Model.Sidebar_Icon_Size;
--        begin
--           Model.Rendered_Assets.Clear;
--
--           for I in 1 .. Stack.Count loop
--              declare
--                 Asset      : constant Carthage.Assets.Asset_Type :=
--                                Stack.Asset (I);
--                 Resource   : constant String :=
--                                "unit"
--                                & Integer'Image (-(Asset.Unit.Index));
--              begin
--                 Renderer.Draw_Rectangle
--                   (X, Y, Icon_Size, Icon_Size,
--                    To_Lui_Color (Background), True);
--                 Renderer.Draw_Image
--                   (X, Y,
--                    Icon_Size, Icon_Size,
--                    Resource);
--                 Model.Rendered_Assets.Append
--                   (Rendered_Asset_Icon'
--                      (Asset => Asset,
--                       Rec   => (X, Y, Icon_Size, Icon_Size)));
--              end;
--
--              X := X + Icon_Size + 2;
--              if X + Icon_Size > Limit_X then
--                 X := Model.Selected_Stack_Layout.X;
--                 Y := Y + Icon_Size + 2;
--              end if;
--           end loop;
--        end Draw_Stack_Icons;

      --------------------------
      -- Draw_Unit_Layer_Tile --
      --------------------------

      procedure Draw_Unit_Layer_Tile
        (Tile               : Carthage.Tiles.Tile_Type;
         Screen_X, Screen_Y : Integer)
      is

         procedure Draw_Stack (Stack : not null access constant
                                 Carthage.Stacks.Stack_Record'Class);

         ----------------
         -- Draw_Stack --
         ----------------

         procedure Draw_Stack (Stack : not null access constant
                                 Carthage.Stacks.Stack_Record'Class)
         is
            Left       : Integer :=
                           Screen_X - Icon_Size / 2;
            Right      : Integer := Left + Icon_Size;
            Top        : Integer :=
                           Screen_Y + Tile_Height / 2
                             - Icon_Size;
            Bottom     : Integer := Top + Icon_Size;
            Background : Carthage.Colors.Color_Type :=
                           Stack.Owner.Color;
            Resource   : constant String :=
                           "unit"
                           & Integer'Image (-(Stack.Asset (1).Unit.Index));
            Size       : constant String :=
                           Ada.Strings.Fixed.Trim
                             (Carthage.Stacks.Asset_Count'Image
                                (Stack.Count),
                              Ada.Strings.Left);
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
                  Left := Left
                    + Integer (Float (Next_X - Screen_X) * Progress);
                  Top := Top
                    + Integer (Float (Next_Y - Screen_Y) * Progress);
                  Right := Left + Icon_Size;
                  Bottom := Top + Icon_Size;
               end;
            end if;

            Model.Rendered_Stacks.Append
              (Rendered_Stack_Icon'
                 (Carthage.Stacks.Stack_Type (Stack),
                  Left, Top, Right, Bottom));

            Background.Alpha := 0.7;

            Renderer.Set_Color (To_Lui_Color (Background));
            Renderer.Rectangle
              (Left, Top, Icon_Size, Icon_Size, True);
            Renderer.Image
              ((Left, Top, Icon_Size, Icon_Size), Resource);
            Renderer.Set_Color (Lui.Colors.Black);
            Renderer.Rectangle
              (Left + Icon_Size - 12, Top + Icon_Size - 8,
               12, 8, True);
            Renderer.Set_Color (Lui.Colors.White);
            Renderer.Text
              (Left + Icon_Size - 10, Top + Icon_Size, Size);
         end Draw_Stack;

      begin
         if Wizard_Mode or else Tile.Currently_Visible_To (Model.House) then
            Tile.Scan_Stacks (Draw_Stack'Access);
         end if;
      end Draw_Unit_Layer_Tile;

   begin

      case Planet_Model_Layer (Layer) is

--           when Static_UI_Layer =>
--
--              for Index in Model.Resource_Layout'Range loop
--                 declare
--                    Rec : Layout_Rectangle renames
--                            Model.Resource_Layout (Index).Rectangle;
--                 begin
--                    Renderer.Draw_Image
--                      (X        => Rec.X + 1,
--                       Y        => Rec.Y + 1,
--                       W        => Resource_Image_Width,
--                       H        => Resource_Image_Height,
--                       Resource =>
--                         "resource" & Integer'Image (-Integer (Index)));
--                 end;
--              end loop;

         when Base_Layer =>

            if Model.Hex_Layout_Changed then
               Model.Hex_Layout.Clear;
            end if;

            Model.Scan_Screen_Tiles (Draw_Base_Layer_Tile'Access);

            Model.Hex_Layout_Changed := False;

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
                  Past_Color : constant Lui.Colors.Color_Type :=
                                 Lui.Colors.To_Color (100, 100, 100);
                  Future_Color : constant Lui.Colors.Color_Type :=
                                   Lui.Colors.To_Color (150, 150, 0);
               begin
                  Renderer.Set_Line_Width (5.0);
                  for Next of Path loop
                     declare
                        X2, Y2 : Integer;
                     begin
                        Model.Get_Screen_Tile_Centre (Next, X2, Y2);
                        if X1 /= 0 then
                           Renderer.Set_Color
                             (if Past then Past_Color else Future_Color);
                           Renderer.Line
                             (X1, Y1, X2, Y2);
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
                  Renderer.Set_Color (Lui.Colors.To_Color (0, 200, 0));
                  Renderer.Rectangle
                    (X      =>
                       Screen_X - Model.Icon_Size / 2,
                     Y      =>
                       Screen_Y + Model.Tile_Height / 2 - Model.Icon_Size,
                     W      => Model.Icon_Size,
                     H      => Model.Icon_Size,
                     Filled => False);
               end if;
            end;
--           when Minimap_Layer =>
--              declare
--                 Scale : constant Positive :=
--                           Integer'Max (Model.Left_Toolbar_Layout.Width
--                                        / Planet_Width,
--                                        1);
--
--                 procedure Draw_Minimap_Tile
--                   (Tile               : Carthage.Tiles.Tile_Type);
--
--                 -----------------------
--                 -- Draw_Minimap_Tile --
--                 -----------------------
--
--                 procedure Draw_Minimap_Tile
--                   (Tile               : Carthage.Tiles.Tile_Type)
--                 is
--                    X     : constant Natural :=
--                              Model.Mini_Map_Layout.X
--                              + Natural (Tile.Position.X) * Scale;
--                    Y     : constant Natural :=
--                              Model.Mini_Map_Layout.Y
--                                + Natural (Tile.Position.Y) * Scale;
--                    Color : constant Carthage.Colors.Color_Type :=
--                              Tile.Base_Terrain.Color
--                                (Model.Planet.Category_Name);
--                 begin
--                    Renderer.Draw_Rectangle
--                      (X      => X,
--                       Y      => Y,
--                       W      => Scale,
--                       H      => Scale,
--                       Color => To_Lui_Color (Color),
--                       Filled => True);
--                 end Draw_Minimap_Tile;
--
--              begin
--                 Draw (Model.Left_Toolbar_Layout, Lui.Colors.Black, True,
--                       Renderer);
--
--                 Model.Planet.Scan_Tiles (Draw_Minimap_Tile'Access);
--
--                 Renderer.Draw_Rectangle
--                   (X      =>
--                      Model.Mini_Map_Layout.X
--                        + Natural (Model.Map_Hex_Left) * Scale,
--                    Y      =>
--                      Model.Mini_Map_Layout.Y
--                        + Natural (Model.Map_Hex_Top) * Scale,
--                    W      => Natural (Model.Map_Hex_Width) * Scale,
--                    H      => Natural (Model.Map_Hex_Height) * Scale,
--                    Color => Lui.Colors.White,
--                    Filled => False);
--
--                 if Model.Selected_Stack /= null then
--                    Draw_Stack_Icons (Model.Selected_Stack);
--                 end if;
--
--              end;
--
--           when UI_Layer =>
--              for Index in Model.Resource_Layout'Range loop
--                 declare
--                    Rec : Layout_Rectangle renames
--                            Model.Resource_Layout (Index).Rectangle;
--                    Q   : constant Natural :=
--                            (if Model.Planet.Has_Palace
--                             then Model.Planet.Palace.Whole_Quantity
--                               (Carthage.Resources.Get (Index))
--                             else 0);
--                 begin
--                    Renderer.Draw_String
--                      (X      => Rec.X + 1,
--                       Y      => Rec.Y + Resource_Image_Height + 4,
--                       Size   => 12,
--                       Color => Lui.Colors.To_Color (98, 207, 62),
--                       Text   => Natural'Image (Q));
--                 end;
--              end loop;
      end case;
   end Render;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (Item          : in out Root_Planet_Model)
   is
   begin
      Carthage.UI.Models.Root_Carthage_Model (Item).Resize;
      Item.Hex_Layout_Changed := True;
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
                       Model.Width / Column_Width + 1;
      Tiles_Down   : constant Natural :=
                       Model.Height / Tile_Height;

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

   begin

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
     (Model : not null access Root_Planet_Model;
      X, Y  : Natural)
   is
   begin
      for Rendered_Stack of Model.Rendered_Stacks loop
         if X in Rendered_Stack.Left .. Rendered_Stack.Right
           and then Y in Rendered_Stack.Top .. Rendered_Stack.Bottom
         then
            Model.Selected_Stack := Rendered_Stack.Stack;
            Model.Set_Render_Layer_Changed (Selection_Layer);
            exit;
         end if;
      end loop;
--        elsif Contains (Model.Mini_Map_Layout, X, Y) then
--           declare
--              Zoomed_Size  : constant Natural :=
--                               Zoomed_Tile_Size (Model.Current_Zoom);
--              Tile_Height  : constant Natural := Zoomed_Size;
--              Column_Width : constant Natural := Zoomed_Size;
--
--              Tiles_Across : constant Positive :=
--                               Model.Map_Pixel_Width / Column_Width + 1;
--              Tiles_Down   : constant Positive :=
--                               Model.Map_Pixel_Height / Tile_Height + 1;
--              Scale        : constant Positive :=
--                           Integer'Max (Model.Left_Toolbar_Layout.Width
--                                        / Planet_Width,
--                                        1);
--           Centre_X : Integer := (X - Model.Mini_Map_Layout.X) / Scale + 1;
--           Centre_Y : Integer := (Y - Model.Mini_Map_Layout.Y) / Scale + 1;
--           begin
--              Centre_X := Integer'Max (Centre_X, Tiles_Across / 2);
--              Centre_Y := Integer'Max (Centre_Y, Tiles_Down / 2);
--              Centre_X := Integer'Min
--                (Centre_X, Planet_Width - Tiles_Across / 2 + 1);
--              Centre_Y := Integer'Min
--                (Centre_Y, Planet_Height - Tiles_Down / 2 + 1);
--              Centre_X := Integer'Max (Centre_X, 1);
--              Centre_Y := Integer'Max (Centre_Y, 1);
--              Centre_X := Integer'Min (Centre_X, Planet_Width);
--              Centre_Y := Integer'Min (Centre_Y, Planet_Height);
--              Model.Set_Centre ((Tile_X (Centre_X), Tile_Y (Centre_Y)));
--              Model.Queue_Render;
--           end;
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
      Model.Hex_Layout_Changed := True;
   end Set_Centre;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Model : Root_Planet_Model;
      X, Y  : Natural)
      return String
   is
      use type Carthage.Tiles.Tile_Type;
      Tile : constant Carthage.Tiles.Tile_Type :=
               Model.Get_Tile (X, Y);
   begin
      if Tile /= null then
         return X'Img & Y'Img & " " & Tile.Description;
      else
         return "";
      end if;
--        elsif Contains (Model.Selected_Stack_Layout, X, Y) then
--           for Info of Model.Rendered_Assets loop
--              if Contains (Info.Rec, X, Y) then
--                 return Info.Asset.Unit.Name;
--              end if;
--           end loop;
--           return "";
--        elsif Contains (Model.Bottom_Toolbar_Layout, X, Y) then
--         for Index in Model.Resource_Layout'Range loop
--           if Contains (Model.Resource_Layout (Index).Rectangle, X, Y) then
--                 return Carthage.Resources.Get (Index).Name;
--              end if;
--           end loop;
--           return "";

   end Tooltip;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Item          : in out Root_Planet_Model)
   is
   begin
      Item.Set_Changed;
   end Update;

   ----------
   -- Zoom --
   ----------

   overriding procedure Zoom
     (Model   : in out Root_Planet_Model;
      Z       : in     Integer;
      X, Y    : in     Integer;
      Control : in     Boolean)
   is
      pragma Unreferenced (Control);
      New_Zoom : constant Integer := Integer (Model.Current_Zoom) + Z;
   begin
      if New_Zoom in Zoom_Level then
         Model.Current_Zoom := New_Zoom;

         if Z > 0 then
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
                  declare
                     Current_X : constant Integer := Integer (Model.Centre.X);
                     Current_Y : constant Integer := Integer (Model.Centre.Y);
                     DX        : constant Integer := Map_X - Current_X;
                     DY        : constant Integer := Map_Y - Current_Y;
                     New_X     : constant Integer :=
                                   Map_X
                                     + (New_Zoom - Zoom_Level'First)
                                   * DX
                                     / (Zoom_Level'Last
                                        - Zoom_Level'First + 1);
                     New_Y     : constant Integer :=
                                   Map_Y
                                     + (New_Zoom - Zoom_Level'First)
                                   * DY
                                     / (Zoom_Level'Last
                                        - Zoom_Level'First + 1);
                  begin
                     Model.Set_Centre ((Tile_X (New_X), Tile_Y (New_Y)));
                  end;
               end if;
            end;
         end if;
         Model.Hex_Layout_Changed := True;
         Model.Set_Changed;
      end if;
   end Zoom;

end Carthage.UI.Models.Planets;
