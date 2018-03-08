with WL.Random;

with Carthage.Calendar;

with Carthage.Combat;
with Carthage.Units;

with Carthage.Updates;

package body Carthage.Stacks.Updates is

   function Next_Arrival_Time
     (Stack : Stack_Type)
      return Carthage.Calendar.Time
     with Pre => Stack.Has_Movement;

   type Stack_Update is
     new Carthage.Updates.Update_Interface with
      record
         Start : Boolean;
         Stack : Stack_Type;
      end record;

   overriding procedure Activate
     (Upd : Stack_Update);

   procedure Start_Movement
     (Stack : in out Stack_Class);

   procedure Execute (Stack : in out Stack_Class);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Upd : Stack_Update)
   is
      use Carthage.Calendar;
   begin
      if Upd.Start then
         Db.Update (Upd.Stack.Reference, Start_Movement'Access);
      else
         Db.Update (Upd.Stack.Reference, Execute'Access);
      end if;

      if Upd.Stack.Has_Movement then
         Upd.Stack.Update.Next_Tile_Start := Carthage.Calendar.Clock;
         Upd.Stack.Update.Next_Tile_Duration :=
           Upd.Stack.Movement_Duration
             (Upd.Stack.Planet.Tile (Upd.Stack.Next_Tile));
         Carthage.Updates.Queue
           (Item       => Stack_Update'(False, Upd.Stack),
            Next_Event => Next_Arrival_Time (Upd.Stack));
      end if;
   end Activate;

   -------------
   -- Execute --
   -------------

   procedure Execute (Stack : in out Stack_Class) is

      function Match (S : not null access constant
                        Stack_Record'Class)
                         return Boolean
      is (Memor."=" (S.Reference, Stack.Reference));

      Ref         : constant Stack_Type :=
                      Stack_Type
                        (Stack.Tile.Find_Stack
                           (Match'Access));

      procedure Check_Hostile
        (Tile        : Carthage.Tiles.Tile_Type;
         Has_Hostile : out Boolean;
         Hostile     : out Carthage.Stacks.Stack_Type);

      procedure Move
        (To   : Tile_Position;
         Stop : out Boolean);

      procedure Attack
        (Hostile : Carthage.Stacks.Stack_Type);

      ------------
      -- Attack --
      ------------

      procedure Attack
        (Hostile : Carthage.Stacks.Stack_Type)
      is
      begin
         Stack.Log
           ("strength"
            & Natural'Image (Stack.Total_Strength)
            & " at " & Stack.Tile.Description
            & " attacking hostile " & Hostile.Identifier
            & " strength"
            & Natural'Image (Hostile.Total_Strength)
            & " in target tile " & Hostile.Tile.Description);

         Carthage.Combat.New_Battle
           (Ref, Hostile, Stack.Planet, Stack.Tile);
      end Attack;

      -------------------
      -- Check_Hostile --
      -------------------

      procedure Check_Hostile
        (Tile        : Carthage.Tiles.Tile_Type;
         Has_Hostile : out Boolean;
         Hostile     : out Carthage.Stacks.Stack_Type)
      is
         procedure Check_Stack
           (Check : not null access constant Stack_Record'Class);

         -------------------
         -- Check_Hostile --
         -------------------

         procedure Check_Stack
           (Check : not null access constant Stack_Record'Class)
         is
         begin
            if Stack.Owner.At_War_With (Check.Owner) then
               if Stack.Manager = null then
                  Stack.Log
                    ("hostile " & Check.Identifier
                     & " on " & Check.Tile.Description
                     & " but no manager");
               else
                  Has_Hostile := True;
                  Hostile     := Carthage.Stacks.Stack_Type (Check);
               end if;
            end if;
         end Check_Stack;
      begin
         Has_Hostile := False;
         Tile.Scan_Stacks (Check_Stack'Access);
      end Check_Hostile;

      ----------
      -- Move --
      ----------

      procedure Move
        (To   : Tile_Position;
         Stop : out Boolean)
      is

         Tile : constant Carthage.Tiles.Tile_Type :=
                  Stack.Planet.Tile (To);

         Cost : constant Float := Stack.Movement_Cost (Tile);

         Hostile     : Carthage.Stacks.Stack_Type;
         Has_Hostile : Boolean;
      begin

         if Cost = 0.0 then
            Stack.Log
              ("stopping because we cannot move to "
               & Tile.Description);
            Stop := True;
            return;
         end if;

         Check_Hostile (Tile, Has_Hostile, Hostile);

         if Has_Hostile then
            Attack (Hostile);
            Stop := True;
            return;
         end if;

         Stop := False;
         Stack.Tile.Update.Remove_Stack (Ref);
         Stack.Tile := Tile;
         Stack.Tile.Update.Add_Stack (Ref);

         declare
            use Carthage.Planets;
            Spotted : Surface_Tiles;
         begin
            Stack.Planet.Get_Tiles
              (Stack.Tile, 0, Stack.Spot + 1,
               null, Spotted);
            for I in 1 .. Tile_Count (Spotted) loop
               declare
                  Tile : constant Carthage.Tiles.Tile_Type :=
                           Get_Tile (Spotted, I);

                  procedure Check_Hostile
                    (Check : not null access constant Stack_Record'Class);

                  -------------------
                  -- Check_Hostile --
                  -------------------

                  procedure Check_Hostile
                    (Check : not null access constant Stack_Record'Class)
                  is
                     Check_Stop : Boolean;
                  begin
                     if Stack.Owner.At_War_With (Check.Owner) then
                        if Stack.Manager = null then
                           Stack.Log
                             ("hostile " & Check.Identifier
                              & " on " & Check.Tile.Description
                              & " but no manager");
                        else
                           Stack.Manager.On_Hostile_Spotted
                             (Ref, Check, Check_Stop);
                        end if;
                        Stop := Stop or else Check_Stop;
                     end if;
                  end Check_Hostile;

               begin
                  Tile.Update.Set_Currently_Visible_To (Stack.Owner);
                  Tile.Scan_Stacks (Check_Hostile'Access);
               end;
            end loop;
         end;

      end Move;

   begin

      if Stack.Current_Path_Index > 0 then
         declare
            Path       : constant Array_Of_Positions :=
                           Stack.Current_Path.Element;
            Path_Index : Positive := Stack.Current_Path_Index;
            Stop       : Boolean := False;
         begin
            Move (Path (Path_Index), Stop);
            Path_Index := Path_Index + 1;

            if Stop then
               Stack.Log ("stopping at " & Stack.Tile.Description);
               Stack.Current_Path_Index := 0;
               Stack.Current_Path.Clear;
            elsif Path_Index > Path'Last then
               Stack.Log ("end of path at " & Stack.Tile.Description);
               Stack.Current_Path_Index := 0;
               Stack.Current_Path.Clear;
            else
               Stack.Log ("waypoint at " & Stack.Tile.Description);
               Stack.Current_Path_Index := Path_Index;

               declare
                  Next_Tile : constant Carthage.Tiles.Tile_Type :=
                                Stack.Planet.Tile (Path (Path_Index));
                  Has_Hostile : Boolean;
                  Hostile     : Carthage.Stacks.Stack_Type;
               begin
                  Check_Hostile (Next_Tile, Has_Hostile, Hostile);
                  if Has_Hostile then
                     Attack (Hostile);
                     Stop := True;
                     Stack.Current_Path_Index := 0;
                     Stack.Current_Path.Clear;
                  else
                     Stack.Next_Tile_Cost :=
                       Stack.Movement_Cost
                         (Stack.Planet.Tile
                            (Path (Path_Index)));
                  end if;
               end;
            end if;
         end;
      end if;
   end Execute;

   -----------------------
   -- Next_Arrival_Time --
   -----------------------

   function Next_Arrival_Time
     (Stack : Stack_Type)
      return Carthage.Calendar.Time
   is
      use Carthage.Calendar;
   begin
      return Clock +
        Stack.Movement_Duration (Stack.Planet.Tile (Stack.Next_Tile));
   end Next_Arrival_Time;

   --------------------
   -- Start_Movement --
   --------------------

   procedure Start_Movement
     (Stack : in out Stack_Class)
   is
      Order : constant Stack_Order_Record :=
                Stack.Orders.First_Element;

      function Passable
        (Tile : Carthage.Tiles.Tile_Type)
            return Boolean
      is (Stack.Can_Enter (Tile));

      function Move_Cost
        (Tile : Carthage.Tiles.Tile_Type)
            return Float
      is (Stack.Movement_Cost (Tile));

   begin
      case Order.Order_Type is
         when Move_To_Tile =>
            declare
               use Carthage.Planets;
               Path : constant Array_Of_Positions :=
                        Stack.Planet.Find_Path
                          (Stack.Tile.Position, Order.Destination,
                           Passable'Access,
                           Move_Cost'Access);
            begin
               if Path'Length = 0 then
                  Stack.Current_Path_Index := 0;
                  Stack.Current_Path.Clear;
               else
                  Stack.Log
                    ("moving to "
                     & Carthage.Tiles.Position_Image
                       (Order.Destination)
                     & ": path length ="
                     & Natural'Image (Path'Length));
                  Stack.Current_Path.Replace_Element (Path);
                  Stack.Current_Path_Index := 1;
               end if;
            end;
         when Move_To_Asset =>
            null;
         when Move_To_Planet =>
            null;
      end case;
   end Start_Movement;

   ------------------
   -- Start_Update --
   ------------------

   procedure Start_Update
     (Stack : Stack_Type)
   is
      use Carthage.Calendar;
      Delay_Duration : constant Duration :=
                         Hours (4)
                         + Hours (Positive (Stack.Count))
                         + Duration (WL.Random.Random_Number (1, 7200));
   begin
      Carthage.Updates.Queue
        (Item       => Stack_Update'(True, Stack),
         Next_Event => Clock + Delay_Duration);
   end Start_Update;

end Carthage.Stacks.Updates;
