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

         procedure Move (Path : Carthage.Planets.Array_Of_Positions);

         ----------
         -- Move --
         ----------

         procedure Move (Path : Carthage.Planets.Array_Of_Positions) is

            Position : constant Tile_Position :=
                         Path (Stack.Current_Path_Index);

            Tile : constant Carthage.Tiles.Tile_Type :=
                     Stack.Planet.Tile (Position);

            Cost : constant Float := Move_Cost (Tile);

            function Match (S : not null access constant
                              Stack_Record'Class)
                                           return Boolean
            is (Memor."=" (S.Reference, Stack.Reference));

            Ref  : constant Stack_Type :=
                     Stack_Type
                       (Stack.Tile.Find_Stack
                          (Match'Access));
         begin
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
                  begin
                     Tile.Update.Set_Currently_Visible_To (Stack.Owner);
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
                        Stack.Current_Path.Replace_Element (Path);
                        Stack.Current_Path_Index := 1;
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
            begin
               while Remaining_Movement > 0.0
                 and then Stack.Current_Path_Index <= Path'Last
               loop
                  Move (Path);
                  Stack.Current_Path_Index :=
                    Stack.Current_Path_Index + 1;
               end loop;
               if Stack.Current_Path_Index > Path'Last then
                  Stack.Current_Path_Index := 0;
               end if;
            end;
         end if;
      end Execute;

   begin
      Db.Iterate (Execute'Access);
   end Execute_Orders;

end Carthage.Stacks.Updates;
