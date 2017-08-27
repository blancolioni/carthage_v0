with WL.String_Maps;

with Carthage.Planets.Configure;

package body Carthage.Galaxy.Configure is

   ---------------------
   -- Configure_Gates --
   ---------------------

   procedure Configure_Gates
     (Gate_Config : Tropos.Configuration)
   is

      function Planet
        (Config : Tropos.Configuration)
         return Carthage.Planets.Planet_Type
      is (if Carthage.Planets.Exists (Config.Config_Name)
          then Carthage.Planets.Get (Config.Config_Name)
          else raise Constraint_Error with
            "gates: " & Config.Config_Name & ": no such planet");

      procedure Append (Planet : Carthage.Planets.Planet_Type);
      procedure Check (Planet : Carthage.Planets.Planet_Type);

      ------------
      -- Append --
      ------------

      procedure Append (Planet : Carthage.Planets.Planet_Type) is
      begin
         Graph.Append (Planet);
      end Append;

      -----------
      -- Check --
      -----------

      procedure Check (Planet : Carthage.Planets.Planet_Type) is
         use type Planet_Graph.Count_Type;

         procedure Check_Reverse_Connection
           (To   : Carthage.Planets.Planet_Type;
            Cost : Float);

         ------------------------------
         -- Check_Reverse_Connection --
         ------------------------------

         procedure Check_Reverse_Connection
           (To   : Carthage.Planets.Planet_Type;
            Cost : Float)
         is
            pragma Unreferenced (Cost);
         begin
            if not Graph.Connected (To.Index, Planet.Index) then
               Planet.Log ("warning: connection from "
                           & Planet.Name & " to " & To.Name
                           & " is one-way");
            end if;
         end Check_Reverse_Connection;

      begin
         if Graph.Edge_Count (Planet.Index) = 0 then
            Planet.Log ("warning: no connections");
         else
            Graph.Iterate_Edges (Planet, Check_Reverse_Connection'Access);
         end if;
      end Check;

   begin
      Carthage.Planets.Scan (Append'Access);

      for From_Config of Gate_Config loop
         declare
            From : constant Carthage.Planets.Planet_Type :=
                     Planet (From_Config);
         begin
            for To_Config of From_Config loop
               declare
                  To : constant Carthage.Planets.Planet_Type :=
                         Planet (To_Config);
               begin
                  Graph.Connect (From.Index, To.Index);
               end;
            end loop;
         end;
      end loop;

      Carthage.Planets.Scan (Check'Access);

   end Configure_Gates;

   -------------------------
   -- Configure_Positions --
   -------------------------

   procedure Configure_Positions
     (Position_Config : Tropos.Configuration)
   is
      package Has_Position_Maps is
        new WL.String_Maps (Boolean);

      Have_Position : Has_Position_Maps.Map;
      Have_Size     : Boolean := False;
      Width, Height : Float;
   begin
      for Config of Position_Config loop
         declare
            X : constant Float := Config.Get (1);
            Y : constant Float := Config.Get (2);
         begin
            if not Have_Size then
               Width := X;
               Height := Y;
               Have_Size := True;
            else
               if Have_Position.Contains (Config.Config_Name) then
                  Carthage.Planets.Get (Config.Config_Name).Log
                    ("warning: repeated position");
               else
                  Have_Position.Insert (Config.Config_Name, True);
                  Carthage.Planets.Configure.Configure_Position
                    (Config.Config_Name,
                     Carthage.Planets.Coordinate (X / Width),
                     Carthage.Planets.Coordinate (Y / Height));
               end if;
            end if;
         end;
      end loop;

      declare
         procedure Check (Planet : Carthage.Planets.Planet_Type);

         -----------
         -- Check --
         -----------

         procedure Check (Planet : Carthage.Planets.Planet_Type) is
         begin
            if not Have_Position.Contains (Planet.Identifier) then
               Planet.Log ("warning: no position information");
            end if;
         end Check;

      begin
         Carthage.Planets.Scan (Check'Access);
      end;

   end Configure_Positions;

end Carthage.Galaxy.Configure;
