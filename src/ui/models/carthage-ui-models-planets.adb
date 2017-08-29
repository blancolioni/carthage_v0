with Lui.Rendering;

with Carthage.Tiles;

with Carthage.UI.Maps;

package body Carthage.UI.Models.Planets is

   Check_Neighbours : constant Boolean := False;

   subtype Zoom_Level is Integer range -3 .. 3;

   Zoomed_Tile_Size : constant array (Zoom_Level) of Positive :=
                        (160, 120, 80, 40, 30, 20, 10);

   Zoomed_Icon_Size : constant array (Zoom_Level) of Positive :=
                        (64, 48, 32, 32, 24, 16, 8);

   type Root_Planet_Model is
     new Root_Carthage_Model with
      record
         Planet       : Carthage.Planets.Planet_Type;
         Centre       : Tile_Position;
         Needs_Render : Boolean := False;
         Current_Zoom : Zoom_Level := 0;
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

   overriding function Select_XY
     (Model : in out Root_Planet_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model
   is (null);

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

   type Planet_Model_Type is access all Root_Planet_Model'Class;

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
            Model.Initialise (Planet.Name);
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
      Draw_Hex_Images : constant Boolean := True;

      Zoomed_Size : constant Natural :=
                      Zoomed_Tile_Size (Model.Current_Zoom);
      Tile_Width   : constant Natural := Zoomed_Size * 6 / 5;
      Tile_Height  : constant Natural := Zoomed_Size;
      Column_Width : constant Natural := Zoomed_Size;
      Row_Height   : constant Natural := Tile_Height + 1;
      Icon_Size    : constant Natural :=
                       Zoomed_Icon_Size (Model.Current_Zoom);

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

      procedure Get_Screen_Tile_Centre
        (Position : Tile_Position;
         X, Y     : out Integer);

      ----------------------------
      -- Get_Screen_Tile_Centre --
      ----------------------------

      procedure Get_Screen_Tile_Centre
        (Position : Tile_Position;
         X, Y     : out Integer)
      is
         Relative_Y : constant Integer :=
                        Integer (Position.Y) - Integer (Model.Centre.Y);
         Relative_X : constant Integer :=
                        Integer (Position.X) - Integer (Model.Centre.X);
         Screen_Y   : constant Integer :=
                        Model.Height / 2 + Relative_Y * Row_Height
                          - (if Position.X mod 2 = 0
                             then Row_Height / 2 else 0);
         Screen_X   : constant Integer :=
                        Model.Width / 2 +
                        (if abs Relative_X <= Planet_Width / 2
                         then Relative_X * Column_Width
                         elsif Relative_X < 0
                         then (Relative_X + Planet_Width) * Column_Width
                         else (Planet_Width - Relative_X) * Column_Width);
      begin
         X := Screen_X;
         Y := Screen_Y;
      end Get_Screen_Tile_Centre;

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

                  Layers   : Carthage.UI.Maps.Tile_Layers;
                  Position : constant Tile_Position :=
                               (X, Tile_Y (Y));

                  Screen_X : Integer;
                  Screen_Y : Integer;

               begin
                  Get_Screen_Tile_Centre (Position, Screen_X, Screen_Y);

                  if Extended_X < 1 then
                     Screen_X := Screen_X - Column_Width * Planet_Width;
                  elsif Extended_X > Planet_Width then
                     Screen_X := Screen_X + Column_Width * Planet_Width;
                  end if;

                  Carthage.UI.Maps.Get_Tile_Layers
                    (Model.Planet, Model.House, Position, Layers);

                  declare
                     use Carthage.UI.Maps;

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
                              if Draw_Hex_Images then
                                 Renderer.Draw_Image
                                   (Screen_X - Tile_Width / 2,
                                    Screen_Y - Tile_Height / 2,
                                    Tile_Width, Tile_Height,
                                    Resource);
                              end if;
                           when Icon =>
                              if Color.Alpha > 0.0 then
                                 Renderer.Draw_Rectangle
                                   (Screen_X - Icon_Size / 2,
                                    Screen_Y + Tile_Height / 2 - Icon_Size,
                                    Icon_Size, Icon_Size,
                                    To_Lui_Colour (Color), True);
                              end if;

                              Renderer.Draw_Image
                                (Screen_X - Icon_Size / 2,
                                 Screen_Y + Tile_Height / 2 - Icon_Size,
                                 Icon_Size, Icon_Size,
                                 Resource);
                        end case;
                     end Draw;

                  begin
                     Layers.Scan_Layers (Draw'Access);
                  end;

               end;
            end loop;
         end if;
      end loop;

      if Check_Neighbours then
         for Y in Top .. Top + Tiles_Down - 1 loop
            if Y in 1 .. Planet_Height then
               for Extended_X in Left .. Left + Tiles_Across - 1 loop
                  declare
                     X      : constant Tile_X :=
                                Tile_X
                                  (if Extended_X < 1
                                   then Extended_X + Planet_Width
                                   elsif Extended_X > Planet_Width
                                   then Extended_X - Planet_Width
                                   else Extended_X);
                     X1, Y1 : Integer;
                  begin
                     Get_Screen_Tile_Centre ((X, Tile_Y (Y)), X1, Y1);

                     for N of Model.Planet.Neighbours ((X, Tile_Y (Y))) loop
                        declare
                           X2, Y2 : Integer;
                        begin
                           Get_Screen_Tile_Centre (N, X2, Y2);
                           Renderer.Draw_Line (X1, Y1, X2, Y2,
                                               Lui.Colours.Black, 1);
                        end;
                     end loop;
                  end;
               end loop;
            end if;
         end loop;
      end if;
   end Render;

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
         return Model.Planet.Tile ((Tile_X (Map_X), Tile_Y (Map_Y)))
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
