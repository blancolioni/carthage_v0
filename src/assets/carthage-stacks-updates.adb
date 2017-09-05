package body Carthage.Stacks.Updates is

   procedure Execute_Orders is

      procedure Execute (Stack : in out Stack_Class);

      -------------
      -- Execute --
      -------------

      procedure Execute (Stack : in out Stack_Class) is
         Remaining_Movement : Float := Float (Stack.Movement);
      begin
         for Order of Stack.Orders loop
            exit when Remaining_Movement = 0.0;

            case Order.Order_Type is
               when Move_To_Tile =>
                  declare
                     use Carthage.Planets;
                     function Move_Cost
                       (Tile : Carthage.Tiles.Tile_Type)
                        return Float
                     is (if Tile.Has_Stack
                         then Float'Last
                         elsif Tile.Has_Road
                         then 1.0
                         else 3.0);

                     Path : constant Array_Of_Positions :=
                              Stack.Planet.Find_Path
                                (Stack.Tile.Position, Order.Destination,
                                 Move_Cost'Access);
                  begin
                     for I in Path'First + 1 .. Path'Last loop
                        declare
                           Cost : constant Float :=
                                    Move_Cost (Stack.Planet.Tile (Path (I)));
                           Ref  : constant Stack_Type :=
                                    Stack.Tile.Stack;
                        begin
                           Stack.Tile.Update.Clear_Stack;
                           Stack.Tile :=
                             Stack.Planet.Tile (Path (I));
                           Stack.Tile.Update.Set_Stack (Ref);

                           Stack.Log ("destination: "
                                      & Carthage.Tiles.Position_Image
                                        (Order.Destination));

                           declare
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
                                    Tile.Update.Set_Currently_Visible_To
                                      (Stack.Owner);
                                 end;
                              end loop;
                           end;

                           if Cost >= Remaining_Movement then
                              Remaining_Movement := 0.0;
                           else
                              Remaining_Movement := Remaining_Movement - Cost;
                           end if;
                        end;
                        exit when Remaining_Movement = 0.0;
                     end loop;
                  end;
               when Move_To_Asset =>
                  null;
               when Move_To_Planet =>
                  null;
            end case;
         end loop;
      end Execute;

   begin
      Db.Iterate (Execute'Access);
   end Execute_Orders;

end Carthage.Stacks.Updates;
