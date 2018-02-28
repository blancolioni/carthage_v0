with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;

with Lui.Colors;
with Lui.Rendering;

with Carthage.Galaxy;
with Carthage.Houses;
with Carthage.Planets;
with Carthage.Stacks;

--  with Carthage.UI.Models.Planets;
--  with Carthage.UI.Models.Stacks;

with Carthage.Paths;

package body Carthage.UI.Models.Galaxy is

   Zoom_Limit    : constant := 5.0;
   Zoom_Duration : constant Duration := 0.8;

   Zoomed_Out_Radius : constant := 24;
   Zoomed_In_Radius  : constant := 60;

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
         Color      : Lui.Colors.Color_Type;
         Capital     : Boolean;
         Colony      : Boolean;
         Connections : Connection_Lists.List;
      end record;

   package Rendered_Planet_Vectors is
     new Ada.Containers.Vectors (Positive, Rendered_Planet);

   type Root_Galaxy_Model is
     new Carthage.UI.Models.Root_Carthage_Model with
      record
         Show_Capital_Names : Boolean := True;
         Show_System_Names  : Boolean := False;
         Rendered_Planets   : Rendered_Planet_Vectors.Vector;
--        Rendered_Stacks    : Carthage.UI.Models.Stacks.Rendered_Stack_List;
         Needs_Render       : Boolean := True;
         Zoomed_To_System   : Boolean := False;
         Zooming_In         : Boolean := False;
         Zooming_Out        : Boolean := False;
         Zoom_Start         : Ada.Calendar.Time;
         Zoom_Level         : Float := 0.0;
         Selected_Planet    : Carthage.Planets.Planet_Type;
      end record;

   overriding procedure Update
     (Model : in out Root_Galaxy_Model);

   overriding procedure Render
     (Model    : in out Root_Galaxy_Model;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class;
      Layer     : Lui.Render_Layer);

   overriding procedure On_Model_Removed
     (Model : in out Root_Galaxy_Model;
      Child : not null access Lui.Models.Root_Object_Model'Class)
   is null;

   overriding procedure Select_XY
     (Model : not null access Root_Galaxy_Model;
      X, Y  : Natural);

   overriding procedure On_Key_Press
     (Model : in out Root_Galaxy_Model;
      Key   : Character);

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

   procedure Load
     (Model : in out Root_Galaxy_Model'Class);

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

   function Interpolate
     (Model      : Root_Galaxy_Model'Class;
      Zoomed_Out : Float;
      Zoomed_In  : Float)
      return Float
   is (Zoomed_In * Model.Zoom_Level + Zoomed_Out * (1.0 - Model.Zoom_Level));

   function Zoomed_Size
     (Model           : Root_Galaxy_Model'Class;
      Zoomed_Out_Size : Integer;
      Zoomed_In_Size  : Integer)
      return Integer
   is (if Model.Zooming_In or else Model.Zooming_Out
       then Integer (Model.Interpolate (Float (Zoomed_In_Size),
                                        Float (Zoomed_Out_Size)))
       elsif Model.Zoomed_To_System then Zoomed_In_Size
       else Zoomed_Out_Size);

   procedure Start_Zoom_In
     (Model  : in out Root_Galaxy_Model'Class;
      Target : Carthage.Planets.Planet_Type);

   procedure Start_Zoom_Out
     (Model  : in out Root_Galaxy_Model'Class);

   procedure Clear_Zoom
     (Model : in out Root_Galaxy_Model'Class);

   --     Unexplored_Color : constant Lui.Colors.Color_Type :=
--                           (0.5, 0.5, 0.5, 0.6);
--     Border_Color     : constant Lui.Colors.Color_Type :=
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

   ----------------
   -- Clear_Zoom --
   ----------------

   procedure Clear_Zoom
     (Model  : in out Root_Galaxy_Model'Class)
   is
   begin
      Model.Zoom_Level := (if Model.Zooming_In then 1.0 else 0.0);
      Model.Zooming_In := False;
      Model.Zooming_Out := False;
   end Clear_Zoom;

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
      Model.Drag_Rotation_Behaviour;

      Model.Set_Eye_Position (0.0, 0.0, 2.2);
      Model.Show_Capital_Names := True;
      Model.Show_System_Names := True;

      Model.Load;

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
--        Link_Color    : constant Lui.Colors.Color_Type :=
--                           (if A_Owner /= null and then B_Owner = A_Owner
--                            then A_Owner.Color
--                            elsif A_Owner = null or else B_Owner = null
--                            then Unexplored_Color
--                            else Border_Color);
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
--        Renderer.Draw_Line (X1, Y1, X2, Y2, Link_Color, Link_Width);
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
      Model : constant Galaxy_Model_Access :=
                new Root_Galaxy_Model;
   begin
      Model.Initialise ("galaxy", 1);
      Model.House := Carthage.Houses.House_Type (House);
      Model.Create_Model;
      return Carthage_Model (Model);
   end Galaxy_Model;

   -------------------------
   -- Get_Screen_Position --
   -------------------------

   procedure Get_Screen_Position
     (Model              : Root_Galaxy_Model'Class;
      X, Y               : Carthage.Planets.Coordinate;
      Screen_X, Screen_Y : out Integer)
   is
      Local_X : Float := Float (X);
      Local_Y : Float := Float (Y);
   begin
      if Model.Zooming_In
        or else Model.Zooming_Out
        or else Model.Zoomed_To_System
      then
         declare
            Zoomed_X : constant Float :=
                         (Float (X) - Float (Model.Selected_Planet.X))
                         * 3.0 + 0.5;
            Zoomed_Y : constant Float :=
                         (Float (Y) - Float (Model.Selected_Planet.Y))
                         * 3.0 + 0.5;
         begin
            if Model.Zooming_In or else Model.Zooming_Out then
               Local_X := Model.Interpolate (Zoomed_X, Local_X);
               Local_Y := Model.Interpolate (Zoomed_Y, Local_Y);
            else
               Local_X := Zoomed_X;
               Local_Y := Zoomed_Y;
            end if;
         end;
      end if;

      Screen_X := Integer (Local_X * Float (Model.Width));
      Screen_Y := Integer (Local_Y * Float (Model.Height));

   end Get_Screen_Position;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Root_Galaxy_Model'Class)
   is
      procedure Append (Planet : Carthage.Planets.Planet_Type);

      ------------
      -- Append --
      ------------

      procedure Append (Planet : Carthage.Planets.Planet_Type) is
         Resource : constant String := Find_Image_Resource (Planet);
         Rec      : Rendered_Planet :=
                      Rendered_Planet'
                        (Index       => Planet.Index,
                         Planet      => Planet,
                         Name        =>
                           Ada.Strings.Unbounded.To_Unbounded_String
                             (Planet.Name),
                         Image       =>
                           Ada.Strings.Unbounded.To_Unbounded_String
                             (Resource),
                         Color      => Lui.Colors.White,
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
   end Load;

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

   ------------------
   -- On_Key_Press --
   ------------------

   overriding procedure On_Key_Press
     (Model : in out Root_Galaxy_Model;
      Key   : Character)
   is
   begin
      if Key = Ada.Characters.Latin_1.ESC then
         if Model.Zoomed_To_System then
            Model.Start_Zoom_Out;
         elsif Model.Zooming_In then
            Model.Clear_Zoom;
         end if;
      end if;
   end On_Key_Press;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model     : in out Root_Galaxy_Model;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class;
      Layer     : Lui.Render_Layer)
   is
      use type Lui.Render_Layer;

      System_Radius : constant Natural :=
                        Model.Zoomed_Size
                          (Zoomed_Out_Radius, Zoomed_In_Radius);
      Reload_Stacks : constant Boolean := True;
--                          Model.Rendered_Stacks.Is_Empty
--                              or else Model.Zooming_In
--                                  or else Model.Zooming_Out;

      procedure Load_Orbital_Stacks
        (Planet : Carthage.Planets.Planet_Type;
         X, Y   : Integer);

      -------------------------
      -- Load_Orbital_Stacks --
      -------------------------

      procedure Load_Orbital_Stacks
        (Planet : Carthage.Planets.Planet_Type;
         X, Y   : Integer)
      is
         Index : Natural := 0;
         subtype Orbital_Stack_Index is Integer range 1 .. 8;

         Icon_Size  : constant Natural :=
                        Model.Zoomed_Size (32, 48);

         procedure Load_Stack (House : Carthage.Houses.House_Type);

         ----------------
         -- Load_Stack --
         ----------------

         procedure Load_Stack (House : Carthage.Houses.House_Type) is
            use Carthage.Stacks;
            Stack : constant Carthage.Stacks.Stack_Type :=
                      Planet.Orbital_Stack (House);
         begin
            Index := Natural'Min (Index + 1, Orbital_Stack_Index'Last);
            if not Stack.Is_Empty then
               declare
                  Left       : constant Integer :=
                                 (case Orbital_Stack_Index (Index) is
                                     when 1 | 4 | 6 =>
                                        X - System_Radius * 4 / 3 - Icon_Size,
                                     when 3 | 5 | 8 =>
                                        X + System_Radius * 4 / 3,
                                     when 2 | 7     =>
                                        X - Icon_Size / 2);
                  Top        : constant Integer :=
                                 (case Orbital_Stack_Index (Index) is
                                     when 1 | 2 | 3 =>
                                        Y - System_Radius * 4 / 3 - Icon_Size,
                                     when 4 | 5    =>
                                        Y - Icon_Size / 2,
                                     when 6 | 7 | 8 =>
                                        Y + System_Radius * 4 / 3);
                  pragma Unreferenced (Left, Top);
               begin
                  null;
--                    Model.Rendered_Stacks.Add_Stack
--                      (Stack  => Stack,
--                       Left   => Left,
--                       Top    => Top,
--                       Width  => Icon_Size,
--                       Height => Icon_Size);
               end;
            end if;
         end Load_Stack;

      begin
         Carthage.Houses.Scan (Load_Stack'Access);
      end Load_Orbital_Stacks;

   begin
      --  Carthage.Updates.Begin_Render;

      if not Have_Bitmaps then
         Load_Bitmaps (Renderer);
      end if;

      if Layer = 1 then
         for Star_Pass in Boolean loop

--              if Star_Pass and then Reload_Stacks then
--                 Model.Rendered_Stacks.Clear;
--              end if;

            for System of Model.Rendered_Planets loop
               declare
                  use type Carthage.Planets.Planet_Type;
                  Screen_X, Screen_Y : Integer;
               begin
                  Model.Get_Screen_Position
                    (System.Planet.X, System.Planet.Y,
                     Screen_X, Screen_Y);

                  if not Model.Zoomed_To_System
                    or else Model.Zooming_In
                    or else Model.Zooming_Out
                    or else Model.Selected_Planet = System.Planet
                    or else Carthage.Galaxy.Connected
                      (System.Planet, Model.Selected_Planet)
                    or else (Screen_X in 1 .. Model.Width
                             and then Screen_Y in 1 .. Model.Height)
                  then

                     if Reload_Stacks then
                        Load_Orbital_Stacks
                          (System.Planet, Screen_X, Screen_Y);
                     end if;

                     if Star_Pass then
                        Renderer.Image
                          (Rec => (X        => Screen_X - System_Radius,
                                   Y        => Screen_Y - System_Radius,
                                   Width    => System_Radius * 2,
                                   Height   => System_Radius * 2),
                           Resource =>
                             Ada.Strings.Unbounded.To_String
                               (System.Image));
                        if System.Colony then
                           Renderer.Set_Color (System.Color);
                           Renderer.Set_Line_Width (2.0);
                           Renderer.Circle
                             (X          => Screen_X,
                              Y          => Screen_Y,
                              Radius     => System_Radius * 2,
                              Filled     => False);

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
                              Renderer.Set_Color
                                (if System.Planet.Has_Owner
                                 then To_Lui_Color
                                   (System.Planet.Owner.Color)
                                 else Lui.Colors.To_Color
                                   (100, 100, 100));
                              Renderer.Set_Font
                                ("Tahoma", 16.0);
                              Renderer.Text
                                (X      => Screen_X - 4 * Name'Length,
                                 Y      => Screen_Y + 42,
                                 Value  => Name);
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
                              Renderer.Set_Color (0.4, 0.4, 0.4, 0.8);
                              Renderer.Set_Line_Width (4.0);
                              Renderer.Line
                                (X1         => Screen_X,
                                 Y1         => Screen_Y,
                                 X2         => To_Screen_X,
                                 Y2         => To_Screen_Y);
                           end;
                        end loop;
                     end if;
                  end if;
               end;
            end loop;

--              if Star_Pass then
--                 if True or else Model.Zoomed_To_System then
--                    Model.Rendered_Stacks.Render (Model, Renderer);
--                 end if;
--              end if;

         end loop;

      end if;

      Model.Needs_Render := False;

--      Carthage.Updates.End_Render;

   end Render;

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Model : not null access Root_Galaxy_Model;
      X, Y  : Natural)
   is
      use Carthage.Planets;

      Planet : constant Planet_Type :=
                 Model.Closest_System
                   (X, Y, Zoomed_In_Radius);
   begin
      if Planet = null then
         if Model.Zoomed_To_System then
            Model.Start_Zoom_Out;
         end if;
      else
         if Model.Zoomed_To_System then
            if Planet = Model.Selected_Planet then
               null;
--                 Model.Push_Model
--                   (Carthage.UI.Models.Planets.Planet_Model
--                      (Model.House, Planet));
            else
               Model.Selected_Planet := Planet;
               Model.Needs_Render := True;
            end if;
         else
            Model.Start_Zoom_In (Planet);
         end if;
      end if;

      Model.Set_Render_Layer_Changed (1);

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

   -------------------
   -- Start_Zoom_In --
   -------------------

   procedure Start_Zoom_In
     (Model  : in out Root_Galaxy_Model'Class;
      Target : Carthage.Planets.Planet_Type)
   is
   begin
      Model.Zooming_In := True;
      Model.Zooming_Out := False;
      Model.Zoom_Start := Ada.Calendar.Clock;
      Model.Zoom_Level := 0.0;
      Model.Selected_Planet := Target;
   end Start_Zoom_In;

   --------------------
   -- Start_Zoom_Out --
   --------------------

   procedure Start_Zoom_Out
     (Model  : in out Root_Galaxy_Model'Class)
   is
   begin
      Model.Zooming_In := False;
      Model.Zooming_Out := True;
      Model.Zoomed_To_System := False;
      Model.Zoom_Level := 1.0;
      Model.Zoom_Start := Ada.Calendar.Clock;
--        Model.Rendered_Stacks.Clear;
   end Start_Zoom_Out;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Model : Root_Galaxy_Model;
      X, Y  : Natural)
      return String
   is
      use Carthage.Planets, Carthage.Stacks;
      Planet : constant Planet_Type :=
                 Model.Closest_System (X, Y, 30);
--        Stack  : constant Stack_Type := null;
--                   Model.Rendered_Stacks.Find_Stack (X, Y);
   begin
--        if Stack /= null then
--           return Stack.Owner.Name;
--        elsif Planet /= null then
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

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Model : in out Root_Galaxy_Model)
   is
   begin
      if Model.Zooming_In or else Model.Zooming_Out then
         declare
            use Ada.Calendar;
         begin
            Model.Zoom_Level :=
              Float (Clock - Model.Zoom_Start) / Float (Zoom_Duration);
            if Model.Zooming_In then
               Model.Zoom_Level := 1.0 - Model.Zoom_Level;
            end if;

            if Model.Zoom_Level not in 0.0 .. 1.0 then
               if Model.Zooming_In then
                  Model.Zoomed_To_System := True;
               else
                  Model.Zoomed_To_System := False;
               end if;
               Model.Clear_Zoom;
            end if;
         end;
         Model.Set_Render_Layer_Changed (1);
      end if;

   end Update;

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
