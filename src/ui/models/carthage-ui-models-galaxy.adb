with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;

with Lui.Colours;
with Lui.Rendering;

with Carthage.Galaxy;
with Carthage.Houses;
with Carthage.Planets;

with Carthage.UI.Models.Planets;

with Carthage.Paths;

package body Carthage.UI.Models.Galaxy is

   Zoom_Limit : constant := 5.0;

   Have_Bitmaps     : Boolean := False;

   procedure Load_Bitmaps
     (Renderer : in out Lui.Rendering.Root_Renderer'Class);

   function Ship_Count_Image
     (Count : Natural)
      return String;

   package Connection_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Positive);

   type Rendered_Planet is
      record
         Index       : Positive;
         Planet      : Carthage.Planets.Planet_Type;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Image       : Ada.Strings.Unbounded.Unbounded_String;
         Radius      : Positive;
         Colour      : Lui.Colours.Colour_Type;
         Capital     : Boolean;
         Colony      : Boolean;
         Connections : Connection_Lists.List;
      end record;

   package Rendered_Planet_Vectors is
     new Ada.Containers.Vectors (Positive, Rendered_Planet);

   type Root_Galaxy_Model is
     new Root_Carthage_Model with
      record
         Show_Capital_Names : Boolean := True;
         Show_System_Names  : Boolean := False;
         Rendered_Planets   : Rendered_Planet_Vectors.Vector;
         Needs_Render       : Boolean := True;
         Zoomed_To_System   : Boolean := False;
         Selected_Planet    : Carthage.Planets.Planet_Type;
      end record;

   overriding function Handle_Update
     (Model    : in out Root_Galaxy_Model)
      return Boolean
   is (Model.Needs_Render);

   overriding procedure Render
     (Model    : in out Root_Galaxy_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding procedure On_Model_Removed
     (Model : in out Root_Galaxy_Model;
      Child : not null access Lui.Models.Root_Object_Model'Class)
   is null;

   overriding function Select_XY
     (Model : in out Root_Galaxy_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model;

   overriding procedure Zoom
     (Model   : in out Root_Galaxy_Model;
      Z       : in     Integer;
      X, Y    : in     Integer;
      Control : in     Boolean);

   overriding function Tooltip
     (Model : Root_Galaxy_Model;
      X, Y  : Natural)
      return String;

   overriding procedure After_Transition
     (Model : in out Root_Galaxy_Model)
   is null;

   overriding procedure Reload
     (Model : in out Root_Galaxy_Model);

   procedure Get_Screen_Position
     (Model              : Root_Galaxy_Model'Class;
      X, Y               : Carthage.Planets.Coordinate;
      Screen_X, Screen_Y : out Integer);

   procedure Create_Model
     (Model : in out Root_Galaxy_Model'Class);

   procedure Draw_Connection
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      A, B     : Positive)
     with Unreferenced;

   procedure Draw_Ships
     (Model         : in out Root_Galaxy_Model'Class;
      Renderer      : in out Lui.Rendering.Root_Renderer'Class;
      Planet        : Carthage.Planets.Planet_Type;
      System_Radius : Positive)
   is null
     with Unreferenced;

   procedure Draw_History
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
     with Unreferenced;

   function Closest_System
     (Model        : Root_Galaxy_Model'Class;
      X, Y         : Integer;
      Max_Distance : Natural)
      return Carthage.Planets.Planet_Type;

   function Find_Image_Resource
     (Planet : Carthage.Planets.Planet_Type)
      return String;

--     Unexplored_Colour : constant Lui.Colours.Colour_Type :=
--                           (0.5, 0.5, 0.5, 0.6);
--     Border_Colour     : constant Lui.Colours.Colour_Type :=
--                           (1.0, 1.0, 1.0, 1.0);

   type Galaxy_Model_Access is access all Root_Galaxy_Model'Class;

   ----------------------
   -- After_Transition --
   ----------------------

--     overriding procedure After_Transition
--       (Model : in out Root_Galaxy_Model)
--     is
--     begin
--        Model.Add_Inline_Model
--          (Width         => Model.Width,
--           Height        => Model.Height,
--           Model         =>
--             Carthage.Systems.Models.System_Model (Model.Selected_System),
--           Attach_Left   => True,
--           Attach_Right  => True,
--           Attach_Top    => True,
--           Attach_Bottom => True);
--     end After_Transition;

   --------------------
   -- Closest_System --
   --------------------

   function Closest_System
     (Model        : Root_Galaxy_Model'Class;
      X, Y         : Integer;
      Max_Distance : Natural)
      return Carthage.Planets.Planet_Type
   is
      use Carthage.Planets;
      Shortest_Distance  : Natural := Natural'Last;
      Closest_Planet     : Planet_Type;
      Screen_X, Screen_Y : Integer;
   begin

      for Render of Model.Rendered_Planets loop
         Model.Get_Screen_Position
           (Render.Planet.X, Render.Planet.Y, Screen_X, Screen_Y);

         if Screen_X in 1 .. Model.Width
           and then Screen_Y in 1 .. Model.Height
           and then abs (X - Screen_X) <= Shortest_Distance
           and then abs (Y - Screen_Y) <= Shortest_Distance
         then
            declare
               D : constant Natural :=
                     (X - Screen_X) ** 2 + (Y - Screen_Y) ** 2;
            begin
               if D < Shortest_Distance then
                  Shortest_Distance := D;
                  Closest_Planet := Render.Planet;
               end if;
            end;
         end if;
      end loop;

      if Max_Distance ** 2 >= Shortest_Distance then
         return Closest_Planet;
      else
         return null;
      end if;
   end Closest_System;

   ------------------
   -- Create_Model --
   ------------------

   procedure Create_Model
     (Model : in out Root_Galaxy_Model'Class)
   is
   begin
      Model.Initialise
        ("Galaxy",
         Last_Render_Layer => 3);

      Model.Drag_Rotation_Behaviour;

      Model.Set_Eye_Position (0.0, 0.0, 2.2);
      Model.Show_Capital_Names := True;
      Model.Show_System_Names := True;

      Model.Reload;

   end Create_Model;

   ---------------------
   -- Draw_Connection --
   ---------------------

   procedure Draw_Connection
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      A, B     : Positive)
   is null;
--        use Carthage.Db;
--
--        use Carthage.Houses;
--        use Carthage.Systems;
--        A_System : constant Star_System_Type := Galaxy_Graph.Vertex (A);
--        B_System : constant Star_System_Type := Galaxy_Graph.Vertex (B);
--        A_Owner  : constant Carthage.Houses.House_Type := A_System.Owner;
--        B_Owner  : constant Carthage.Houses.House_Type := B_System.Owner;
--
--        Link_Colour    : constant Lui.Colours.Colour_Type :=
--                           (if A_Owner /= null and then B_Owner = A_Owner
--                            then A_Owner.Colour
--                            elsif A_Owner = null or else B_Owner = null
--                            then Unexplored_Colour
--                            else Border_Colour);
--        Link_Width     : constant Positive :=
--                           Natural'Min
--                             (A_System.Traffic (B_System)
--                              + B_System.Traffic (A_System),
--                              5)
--                           + 1;
--        X1, X2, Y1, Y2 : Integer;
--     begin
--        Model.Star_System_Screen (A_System, X1, Y1);
--        Model.Star_System_Screen (B_System, X2, Y2);
--        Renderer.Draw_Line (X1, Y1, X2, Y2, Link_Colour, Link_Width);
--     end Draw_Connection;

   ------------------
   -- Draw_History --
   ------------------

   procedure Draw_History
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is null;

   -------------------------
   -- Find_Image_Resource --
   -------------------------

   function Find_Image_Resource
     (Planet : Carthage.Planets.Planet_Type)
      return String
   is
      function Exists (Resource : String) return Boolean;

      function Exists (Resource : String) return Boolean
      is (Ada.Directories.Exists
          (Carthage.Paths.Config_File
           ("images/" & Resource & ".png")));

      Id_Resource : constant String :=
                      "planets/" & Planet.Identifier;
      Category_Resource : constant String :=
                            "planets/" & Planet.Category.Identifier;
   begin
      if Exists (Id_Resource) then
         return Id_Resource;
      elsif Exists (Category_Resource) then
         return Category_Resource;
      else
         return "planets/normal";
      end if;
   end Find_Image_Resource;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
     (House : not null access constant Carthage.Houses.House_Class)
      return Carthage_Model
   is
      use Lui.Models;

   begin
      if not Have_Model (House, "galaxy") then
         declare
            Model : constant Galaxy_Model_Access :=
                      new Root_Galaxy_Model;
         begin
            Model.Create_Model;
            Set_Model (House, "galaxy", Model);
         end;
      end if;

      return Get_Model (House, "galaxy");
   end Galaxy_Model;

   -------------------------
   -- Get_Screen_Position --
   -------------------------

   procedure Get_Screen_Position
     (Model              : Root_Galaxy_Model'Class;
      X, Y               : Carthage.Planets.Coordinate;
      Screen_X, Screen_Y : out Integer)
   is
   begin
      if Model.Zoomed_To_System then
         declare
            Local_X : constant Float :=
                        (Float (X) - Float (Model.Selected_Planet.X))
                        * 3.0 + 0.5;
            Local_Y : constant Float :=
                        (Float (Y) - Float (Model.Selected_Planet.Y))
                        * 3.0 + 0.5;
         begin
            Screen_X := Integer (Local_X * Float (Model.Width));
            Screen_Y := Integer (Local_Y * Float (Model.Height));
         end;
      else
         Screen_X := Integer (Float (X) * Float (Model.Width));
         Screen_Y := Integer (Float (Y) * Float (Model.Height));
      end if;
   end Get_Screen_Position;

   ------------------
   -- Load_Bitmaps --
   ------------------

   procedure Load_Bitmaps
     (Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      procedure Save_Resource
        (Resource_Name : String;
         Image         : WL.Images.Image_Type'Class);

      -------------------
      -- Save_Resource --
      -------------------

      procedure Save_Resource
        (Resource_Name : String;
         Image         : WL.Images.Image_Type'Class)
      is
      begin
         Renderer.Create_Image_Resource
           (Resource_Name, Image);
      end Save_Resource;

   begin
      Carthage.UI.Load_Resources
        (Save_Resource'Access);
      Have_Bitmaps := True;
   end Load_Bitmaps;

   ------------
   -- Reload --
   ------------

   overriding procedure Reload
     (Model : in out Root_Galaxy_Model)
   is
      procedure Append (Planet : Carthage.Planets.Planet_Type);

      ------------
      -- Append --
      ------------

      procedure Append (Planet : Carthage.Planets.Planet_Type) is
         Resource : constant String := Find_Image_Resource (Planet);
         Rec    : Rendered_Planet :=
                    Rendered_Planet'
                      (Index       => Planet.Index,
                       Planet      => Planet,
                       Name        =>
                         Ada.Strings.Unbounded.To_Unbounded_String
                           (Planet.Name),
                       Image       =>
                         Ada.Strings.Unbounded.To_Unbounded_String
                           (Resource),
                       Radius      => 30,
                       Colour      => Lui.Colours.White,
                       Colony      => False,
                       Capital     => False,
                       Connections => <>);

         procedure Add_Connection (To : Carthage.Planets.Planet_Type);

         --------------------
         -- Add_Connection --
         --------------------

         procedure Add_Connection (To : Carthage.Planets.Planet_Type) is
         begin
            Rec.Connections.Append
              (To.Index);
         end Add_Connection;

      begin
         Carthage.Galaxy.Scan_Connections
           (Planet, Add_Connection'Access);
         Model.Rendered_Planets.Append (Rec);
      end Append;

   begin
      Model.Rendered_Planets.Clear;
      Carthage.Planets.Scan (Append'Access);
   end Reload;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_Galaxy_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      use type Lui.Rendering.Render_Layer;
   begin
      --  Carthage.Updates.Begin_Render;

      if not Have_Bitmaps then
         Load_Bitmaps (Renderer);
      end if;

      if Renderer.Current_Render_Layer = 1 then
         for Star_Pass in Boolean loop
            for System of Model.Rendered_Planets loop
               declare
                  use type Carthage.Planets.Planet_Type;
                  Screen_X, Screen_Y : Integer;
               begin
                  Model.Get_Screen_Position
                    (System.Planet.X, System.Planet.Y,
                     Screen_X, Screen_Y);

                  if not Model.Zoomed_To_System
                    or else Model.Selected_Planet = System.Planet
                    or else Carthage.Galaxy.Connected
                      (System.Planet, Model.Selected_Planet)
                    or else (Screen_X in 1 .. Model.Width
                             and then Screen_Y in 1 .. Model.Height)
                  then
                     if Star_Pass then
                        Renderer.Draw_Image
                          (X        => Screen_X - System.Radius,
                           Y        => Screen_Y - System.Radius,
                           W        => System.Radius * 2,
                           H        => System.Radius * 2,
                           Resource =>
                             Ada.Strings.Unbounded.To_String
                               (System.Image));
                        if System.Colony then
                           Renderer.Draw_Circle
                             (X          => Screen_X,
                              Y          => Screen_Y,
                              Radius     => System.Radius * 2,
                              Colour     => System.Colour,
                              Filled     => False,
                              Line_Width => 2);

                        end if;

                        declare
                           Name : constant String :=
                                    Ada.Strings.Unbounded.To_String
                                      (System.Name);
                        begin
                           if Model.Show_System_Names
                             or else (System.Capital
                                      and then Model.Show_Capital_Names)
                           then
                              Renderer.Draw_String
                                (X      => Screen_X - 4 * Name'Length,
                                 Y      => Screen_Y + 42,
                                 Size   => 16,
                                 Colour =>
                                   (if System.Planet.Has_Owner
                                    then To_Lui_Colour
                                      (System.Planet.Owner.Colour)
                                    else Lui.Colours.To_Colour
                                      (100, 100, 100)),
                                 Text   => Name);
                           end if;
                        end;
                     else

                        for Connection of System.Connections loop
                           declare
                              use Carthage.Planets;
                              To          : Rendered_Planet renames
                                              Model.Rendered_Planets
                                                (Connection);
                              To_X        : constant Coordinate := To.Planet.X;
                              To_Y        : constant Coordinate := To.Planet.Y;
                              To_Screen_X : Integer;
                              To_Screen_Y : Integer;
                           begin
                              Model.Get_Screen_Position
                                (To_X, To_Y, To_Screen_X, To_Screen_Y);
                              Renderer.Draw_Line
                                (X1         => Screen_X,
                                 Y1         => Screen_Y,
                                 X2         => To_Screen_X,
                                 Y2         => To_Screen_Y,
                                 Colour     =>
                                   Lui.Colours.To_Colour (100, 100, 100),
                                 Line_Width => 1);
                           end;
                        end loop;
                     end if;
                  end if;
               end;
            end loop;
         end loop;
      end if;

      Model.Needs_Render := False;

--      Carthage.Updates.End_Render;

   end Render;

   ---------------
   -- Select_XY --
   ---------------

   overriding function Select_XY
     (Model : in out Root_Galaxy_Model;
      X, Y  : Natural)
      return Lui.Models.Object_Model
   is
      use Carthage.Planets;

      Planet : constant Planet_Type :=
                 Model.Closest_System (X, Y, 30);
   begin
      if Planet = null then
         if Model.Zoomed_To_System then
            Model.Zoomed_To_System := False;
            Model.Needs_Render := True;
         end if;
         return null;
      else
         if Model.Zoomed_To_System then
            if Planet = Model.Selected_Planet then
               return Lui.Models.Object_Model
                 (Carthage.UI.Models.Planets.Planet_Model
                    (Model.House, Planet));
            else
               Model.Selected_Planet := Planet;
               Model.Needs_Render := True;
               return null;
            end if;
         else
            Model.Zoomed_To_System := True;
            Model.Selected_Planet := Planet;
            Model.Needs_Render := True;
            return null;
         end if;
      end if;

   end Select_XY;

   ----------------------
   -- Ship_Count_Image --
   ----------------------

   function Ship_Count_Image
     (Count : Natural)
      return String
   is
   begin
      if Count < 10 then
         return Result : constant String (1 .. Count) := (others => 'I') do
            null;
         end return;
      elsif Count < 100 then
         declare
            Xs : constant String (1 .. Count / 10) := (others => 'X');
         begin
            return Xs & Ship_Count_Image (Count mod 10);
         end;
      else
         declare
            Result : constant String := Natural'Image (Count);
         begin
            return Result (2 .. Result'Last);
         end;
      end if;
   end Ship_Count_Image;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Model : Root_Galaxy_Model;
      X, Y  : Natural)
      return String
   is
      use Carthage.Planets;
      Planet : constant Planet_Type :=
                 Model.Closest_System (X, Y, 30);
   begin
      if Planet /= null then
         if Planet.Has_Owner then
            return Planet.Name & " owned by " & Planet.Owner.Full_Name;
         else
            return Planet.Name;
         end if;
      else
         return "";
      end if;
   end Tooltip;

   ----------
   -- Zoom --
   ----------

   overriding procedure Zoom
     (Model   : in out Root_Galaxy_Model;
      Z       : in     Integer;
      X, Y    : in     Integer;
      Control : in     Boolean)
   is
   begin
      Lui.Models.Root_Object_Model (Model).Zoom (Z, X, Y, Control);
      if Model.Eye_Z > Zoom_Limit then
         Model.Set_Eye_Position (Model.Eye_X, Model.Eye_Y, Zoom_Limit);
      end if;
   end Zoom;

end Carthage.UI.Models.Galaxy;
