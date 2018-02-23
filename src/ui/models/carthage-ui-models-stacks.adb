with Ada.Strings.Fixed;

package body Carthage.UI.Models.Stacks is

   ---------------
   -- Add_Stack --
   ---------------

   procedure Add_Stack
     (List          : in out Rendered_Stack_List'Class;
      Stack         : Carthage.Stacks.Stack_Type;
      Left, Top     : Integer;
      Width, Height : Natural)
   is
   begin
      List.Append
        (Rendered_Stack_Record'
           (Stack  => Stack,
            Left   => Left,
            Top    => Top,
            Width  => Width,
            Height => Height));
   end Add_Stack;

   -----------
   -- Clear --
   -----------

   procedure Clear (List : in out Rendered_Stack_List'Class) is
   begin
      Rendered_Stack_Lists.List (List).Clear;
   end Clear;

   ----------------
   -- Find_Stack --
   ----------------

   function Find_Stack
     (List : Rendered_Stack_List'Class;
      X, Y : Integer)
      return Carthage.Stacks.Stack_Type
   is
   begin
      for Rec of List loop
         if X in Rec.Left .. Rec.Left + Rec.Width
           and then Y in Rec.Top .. Rec.Top + Rec.Height
         then
            return Rec.Stack;
         end if;
      end loop;
      return null;
   end Find_Stack;

   ------------
   -- Render --
   ------------

   procedure Render
     (List     : Rendered_Stack_List'Class;
      Model    : Root_Carthage_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      pragma Unreferenced (Model);
   begin
      for Rec of List loop
         declare
            House      : constant Carthage.Houses.House_Type :=
                           Rec.Stack.Owner;
            Background : Carthage.Colours.Colour_Type :=
                           House.Colour;
            Resource   : constant String :=
                           (if Rec.Stack.Is_Empty then ""
                            else Asset_Resource (Rec.Stack.Asset (1)));
            Size_Text  : constant String :=
                           Ada.Strings.Fixed.Trim
                             (Carthage.Stacks.Asset_Count'Image
                                (Rec.Stack.Count),
                              Ada.Strings.Left);
            Icon_Size  : constant Natural :=
                           Natural'Min (Rec.Width, Rec.Height);
         begin
            Background.Alpha := 0.7;

            Renderer.Draw_Rectangle
              (Rec.Left, Rec.Top, Rec.Width, Rec.Height,
               To_Lui_Colour (Background), True);
            if not Rec.Stack.Is_Empty then
               Renderer.Draw_Image
                 (Rec.Left, Rec.Top, Icon_Size, Icon_Size, Resource);
            end if;
            Renderer.Draw_Rectangle
              (Rec.Left + Rec.Width - 12, Rec.Top + Rec.Height - 8,
               12, 8, Lui.Colours.Black, True);
            Renderer.Draw_String
              (Rec.Left + Rec.Width - 10, Rec.Top + Rec.Height, 8,
               Lui.Colours.White, Size_Text);
         end;
      end loop;
   end Render;

end Carthage.UI.Models.Stacks;
