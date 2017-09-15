with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Lui.Rendering;

with Carthage.Stacks;
with Carthage.Tiles;

with Carthage.UI.Maps;

package body Carthage.UI.Models.Planets is

   subtype Zoom_Level is Integer range -3 .. 3;

   Zoomed_Tile_Size : constant array (Zoom_Level) of Positive :=
                        (160, 120, 80, 40, 30, 20, 10);

   Zoomed_Icon_Size : constant array (Zoom_Level) of Positive :=
                        (64, 48, 32, 32, 24, 16, 8);

   Base_Layer      : constant Lui.Rendering.Render_Layer := 1;
   Unit_Layer      : constant Lui.Rendering.Render_Layer := 2;
   Path_Layer      : constant Lui.Rendering.Render_Layer := 3;
   Selection_Layer : constant Lui.Rendering.Render_Layer := 4;

   Last_Layer      : constant Lui.Rendering.Render_Layer := Selection_Layer;

   subtype Planet_Model_Layer is
     Lui.Rendering.Render_Layer range 1 .. Last_Layer;

   type Rendered_Stack_Icon is
      record
         Stack : Carthage.Stacks.Stack_Type;
         Left, Top, Right, Bottom : Integer;
      end record;

   package Rendered_Stack_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rendered_Stack_Icon);

   type Root_Planet_Model is
     new Root_Carthage_Model with
      record
         Planet          : Carthage.Planets.Planet_Type;
         Centre          : Tile_Position;
         Selected_Stack  : Carthage.Stacks.Stack_Type;
         Rendered_Stacks : Rendered_Stack_Lists.List;
         Needs_Render    : Boolean := False;
         Current_Zoom    : Zoom_Level := 0;
      end record;

   overriding function Handle_Update
     (Model    : in out Root_Planet_Model)
      return Boolean
   is (Model.Needs_Render);

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
                     Model.Height / 2 + Relative_Y * Model.Row_Height
                       - (if Position.X mod 2 = 0
                          then Model.Row_Height / 2 else 0);
      Screen_X   : constant Integer :=
                     Model.Width / 2 +
                       (if abs Relative_X <= Planet_Width / 2
                        then Relative_X * Model.Column_Width
                        elsif Relative_X < 0
                        then (Relative_X + Planet_Width) * Model.Column_Width
                        else (Planet_Width - Relative_X) * Model.Column_Width);
   begin
      X := Screen_X;
      Y := Screen_Y;
   end Get_Screen_Tile_Centre;

   ------------------
   -- Planet_Model --
   ------------------

   function Planet_Model
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Carthage_Model
   is
   begin
      if not Have_Model (House, Planet.Identifier) then
         declare
            use Carthage.Planets;
            Model : constant Planet_Model_Type := new Root_Planet_Model;
         begin
            Model.Initialise (Planet.Name, Last_Render_Layer => Last_Layer);
            Model.Set_Background
              (Lui.Colours.To_Colour (200, 200, 200));
            Model.Planet := Planet;
            Model.Centre := (Planet_Width / 2, Planet_Height / 2);
            Set_Model (House, Planet.Identifier, Model);
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

      end Draw_Base_Layer_Tile;

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
               Left       : constant Integer :=
                              Screen_X - Icon_Size / 2;
               Right      : constant Integer := Left + Icon_Size;
               Top        : constant Integer :=
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

      end case;
      Model.Needs_Render := False;
   end Render;

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

      Tiles_Across : constant Positive := Model.Width / Column_Width + 1;
      Tiles_Down   : constant Positive := Model.Height / Tile_Height + 1;

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
     (Model : in out Root_Planet_Model;
      X, Y  : Natural)
   is
   begin
      Ada.Text_IO.Put_Line ("select:" & X'Img & Y'Img);
      for Rendered_Stack of Model.Rendered_Stacks loop
         if X in Rendered_Stack.Left .. Rendered_Stack.Right
           and then Y in Rendered_Stack.Top .. Rendered_Stack.Bottom
         then
            Ada.Text_IO.Put_Line ("selecting: "
                                  & Rendered_Stack.Stack.Identifier
                                  & ": " & Rendered_Stack.Stack.Description);
            Model.Selected_Stack := Rendered_Stack.Stack;
            Model.Queue_Render;
            exit;
         end if;
      end loop;
   end Select_XY;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Model : Root_Planet_Model;
      X, Y  : Natural)
      return String
   is
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
