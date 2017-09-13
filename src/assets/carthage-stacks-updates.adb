with Carthage.Combat;
with Carthage.Units;

package body Carthage.Stacks.Updates is

   procedure Execute_Orders is

      procedure Execute (Stack : in out Stack_Class);

      -------------
      -- Execute --
      -------------

      procedure Execute (Stack : in out Stack_Class) is

         function Move_Cost
           (Tile : Carthage.Tiles.Tile_Type)
                        return Float
         is (if Tile.Has_Road
             then 1.0
             else 3.0);

         Remaining_Movement : Float := Float (Stack.Movement);

         procedure Check_Hostile
           (Tile        : Carthage.Tiles.Tile_Type;
            Has_Hostile : out Boolean;
            Hostile     : out Carthage.Stacks.Stack_Type);

         procedure Move
           (To   : Tile_Position;
            Stop : out Boolean);

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

            Cost : constant Float := Move_Cost (Tile);

            function Match (S : not null access constant
                              Stack_Record'Class)
                                           return Boolean
            is (Memor."=" (S.Reference, Stack.Reference));

            Ref  : constant Stack_Type :=
                     Stack_Type
                       (Stack.Tile.Find_Stack
                          (Match'Access));
            Hostile     : Carthage.Stacks.Stack_Type;
            Has_Hostile : Boolean;
         begin

            Check_Hostile (Tile, Has_Hostile, Hostile);

            if Has_Hostile then
               Stack.Log
                 ("at " & Stack.Tile.Description
                  & " attacking hostile " & Hostile.Identifier
                  & " in target tile " & Tile.Description);
               Carthage.Combat.New_Battle
                 (Ref, Hostile, Stack.Planet, Stack.Tile);

--                 declare
--                    use Carthage.Combat;
--                    Battle : Battle_Record;
--                 begin
--                    Stack.Planet.Log ("Starting the Battle of "
--                                      & Stack.Planet.Name);
--                    Create (Battle, Stack.Owner, Hostile.Owner);
--                    Add_Stack (Battle, Ref);
--                    Add_Stack (Battle, Hostile);
--                    for Weapon in Carthage.Units.Weapon_Category loop
--                       declare
--                          Round : constant Attack_Record_Array :=
--                                    Attack_Round (Battle, Weapon);
--                       begin
--                          for Attack of Round loop
--                             Stack.Planet.Log (Image (Attack));
--                          end loop;
--                       end;
--                    end loop;

               Stop := True;
               --  end;
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

            if Cost >= Remaining_Movement then
               Remaining_Movement := 0.0;
            else
               Remaining_Movement := Remaining_Movement - Cost;
            end if;
         end Move;

      begin
         if Stack.Current_Path_Index = 0
           and then not Stack.Orders.Is_Empty
         then
            declare
               Order : constant Stack_Order_Record :=
                         Stack.Orders.First_Element;
            begin
               case Order.Order_Type is
                  when Move_To_Tile =>
                     declare
                        use Carthage.Planets;
                        Path : constant Array_Of_Positions :=
                                 Stack.Planet.Find_Path
                                   (Stack.Tile.Position, Order.Destination,
                                    Move_Cost'Access);
                     begin
                        Stack.Log ("moving to "
                                   & Carthage.Tiles.Position_Image
                                     (Order.Destination)
                                   & ": path length ="
                                   & Natural'Image (Path'Length));
                        Stack.Current_Path.Replace_Element (Path);
                        Stack.Current_Path_Index := 2;
                     end;
                  when Move_To_Asset =>
                     null;
                  when Move_To_Planet =>
                     null;
               end case;
            end;
         end if;

         if Stack.Current_Path_Index > 0 then
            declare
               Path : constant Carthage.Planets.Array_Of_Positions :=
                        Stack.Current_Path.Element;
               Path_Index : Positive := Stack.Current_Path_Index;
               Stop : Boolean := False;
            begin
               while not Stop and then Remaining_Movement > 0.0
                 and then Path_Index <= Path'Last
               loop
                  Move (Path (Path_Index), Stop);
                  Path_Index := Path_Index + 1;
               end loop;

               if Stop
                 or else Path_Index > Path'Last
               then
                  Stack.Log ("stopping at " & Stack.Tile.Description);
                  Stack.Current_Path_Index := 0;
                  Stack.Current_Path.Clear;
               else
                  Stack.Log ("waypoint at " & Stack.Tile.Description);
                  Stack.Current_Path_Index := Path_Index;
               end if;
            end;
         end if;
      end Execute;

   begin
      Db.Iterate (Execute'Access);
   end Execute_Orders;

end Carthage.Stacks.Updates;
