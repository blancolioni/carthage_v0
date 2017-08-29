with Carthage.Terrain;

package body Carthage.Planets is

   ----------------------
   -- Clear_Visibility --
   ----------------------

   procedure Clear_Visibility (Planet : in out Planet_Class) is
   begin
      for Tile of Planet.Tiles loop
         Carthage.Tiles.Clear_Visibility (Tile);
      end loop;
   end Clear_Visibility;

   ---------------
   -- Find_Path --
   ---------------

   function Find_Path
     (Planet : Planet_Record;
      Start  : Tile_Position;
      Finish : Tile_Position;
      Cost   : not null access
        function (Tile : Carthage.Tiles.Tile_Type)
      return Float)
      return Array_Of_Positions
   is
      function Local_Cost (From, To : Tile_Position) return Float
      is (Cost (Planet.Tile (To)));

      Path : constant Tile_Graphs.Path :=
               Surface_Graph.Shortest_Path
                 (Index_Of (Start), Index_Of (Finish), Local_Cost'Access);
      Vs   : constant Tile_Graphs.Array_Of_Vertices :=
               Tile_Graphs.Get_Path (Path);
   begin
      return Result : Array_Of_Positions (Vs'Range) do
         for I in Result'Range loop
            Result (I) := Surface_Graph.Vertex (Vs (I));
         end loop;
      end return;
   end Find_Path;

   ---------------
   -- Find_Tile --
   ---------------

   function Find_Tile
     (Planet : Planet_Record;
      Start  : Tile_Position;
      Test   : not null access
        function (Position : Tile_Position) return Boolean)
      return Tile_Position
   is
      pragma Unreferenced (Planet);
   begin
      return Surface_Graph.Breadth_First_Search (Start, Test);
   end Find_Tile;

   ---------------
   -- Get_Tiles --
   ---------------

   procedure Get_Tiles (Planet : not null access constant Planet_Record'Class;
                        Tiles  : out Surface_Tiles)
   is
      function OK (Tile : Carthage.Tiles.Tile_Type) return Boolean
      is (True);

   begin
      Planet.Get_Tiles (OK'Access, Tiles);
   end Get_Tiles;

   ---------------
   -- Get_Tiles --
   ---------------

   procedure Get_Tiles (Planet : not null access constant Planet_Record'Class;
                        Test   : not null access
                          function (Tile : Carthage.Tiles.Tile_Type)
                        return Boolean;
                        Tiles  : out Surface_Tiles)
   is
   begin
      Tiles.Planet := Planet_Type (Planet);
      for Tile of Planet.Tiles loop
         if Test (Tile) then
            Tiles.Tiles.Append (Tile);
         end if;
      end loop;
   end Get_Tiles;

   ---------------
   -- Get_Tiles --
   ---------------

   procedure Get_Tiles
     (Planet       : not null access constant Planet_Record'Class;
      Origin       : Carthage.Tiles.Tile_Type;
      Min_Distance : Natural;
      Max_Distance : Natural;
      Test         : access
        function (Tile : Carthage.Tiles.Tile_Type)
      return Boolean;
      Tiles        : out Surface_Tiles)
   is
      Distance : array (Tile_X, Tile_Y) of Integer :=
                   (others => (others => -1));
      Queue    : Tile_Vectors.Vector;
      Start    : Positive := 1;
      Last     : Positive;
   begin
      Tiles.Planet := Planet_Type (Planet);
      Queue.Append (Origin);
      Distance (Origin.Position.X, Origin.Position.Y) := 0;

      for I in 1 .. Max_Distance loop
         Last := Queue.Last_Index;

         for J in Start .. Last loop
            declare
               T : constant Carthage.Tiles.Tile_Type := Queue (J);
               P : constant Tile_Position := T.Position;
               Current_D : constant Integer := Distance (P.X, P.Y);
            begin
               if Current_D in Min_Distance .. Max_Distance + 1 then
                  Tiles.Tiles.Append (Planet.Tile (P));
               end if;

               for N of Planet.Neighbours (P) loop
                  if Test = null
                    or else Test (Planet.Tile (N))
                  then
                     declare
                        D : Integer renames Distance (N.X, N.Y);
                     begin
                        if D = -1
                          or else Current_D + 1 < D
                        then
                           D := Current_D + 1;
                           if D < Max_Distance then
                              Queue.Append (Planet.Tile (N));
                           end if;
                        end if;

                     end;
                  end if;
               end loop;
            end;
         end loop;
         Start := Last + 1;
      end loop;
   end Get_Tiles;

   ------------------
   -- Hex_Distance --
   ------------------

   function Hex_Distance
     (From, To : Tile_Position)
      return Natural
   is
   begin
      return abs (Integer (From.X - To.X))
        + abs (Integer (From.Y - To.Y));
--        return Tile_Graphs.Vertex_Count
--          (Surface_Graph.Shortest_Path (Index_Of (From), Index_Of (To)));
   end Hex_Distance;

   ---------------------
   -- Land_Connection --
   ---------------------

   function Land_Connection
     (Planet   : Planet_Record;
      From, To : Tile_Position)
      return Boolean
   is
      function Is_Land (Position : Tile_Position) return Boolean
      is (not Planet.Tile (Position).Is_Water);

      Path : constant Tile_Graphs.Path :=
               Surface_Graph.Shortest_Path
                 (Index_Of (From), Index_Of (To), Is_Land'Access);
   begin
      return From = To
        or else Tile_Graphs.Vertex_Count (Path) > 1;
   end Land_Connection;

   ---------------------
   -- Neighbour_Tiles --
   ---------------------

   function Neighbour_Tiles
     (Planet   : Planet_Record;
      Position : Tile_Position)
      return Array_Of_Tiles
   is
      Positions : constant Array_Of_Positions := Planet.Neighbours (Position);
      Tiles     : Array_Of_Tiles (Positions'Range);
   begin
      for I in Positions'Range loop
         Tiles (I) := Planet.Tile (Positions (I));
      end loop;
      return Tiles;
   end Neighbour_Tiles;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (Planet   : Planet_Record;
      Position : Tile_Position)
      return Array_Of_Positions
   is
      pragma Unreferenced (Planet);

      Result : Array_Of_Positions (1 .. 6);
      Count  : Natural := 0;

      procedure Add_Neighbour
        (Neighbour : Tile_Position;
         Cost      : Float);

      -------------------
      -- Add_Neighbour --
      -------------------

      procedure Add_Neighbour
        (Neighbour : Tile_Position;
         Cost      : Float)
      is
         pragma Unreferenced (Cost);
      begin
         Count := Count + 1;
         Result (Count) := Neighbour;
      end Add_Neighbour;

   begin
      Surface_Graph.Iterate_Edges
        (Position, Add_Neighbour'Access);
      return Result (1 .. Count);
   end Neighbours;

   -----------------
   -- Remove_Tile --
   -----------------

   procedure Remove_Tile
     (Tiles    : in out Surface_Tiles;
      Position : Tile_Position)
   is
      use type Ada.Containers.Count_Type;
      Found : Boolean := False;
   begin
      if Tiles.Tiles.Is_Empty then
         return;
      end if;

      declare
         Last  : constant Carthage.Tiles.Tile_Type :=
                   Tiles.Tiles.Last_Element;
      begin
         if Last.Position = Position then
            Found := True;
         else
            for Tile of Tiles.Tiles loop
               if Tile.Position = Position then
                  Found := True;
                  Tile := Last;
               end if;
            end loop;
         end if;

         if Found then
            Tiles.Tiles.Set_Length (Tiles.Tiles.Length - 1);
         end if;
      end;
   end Remove_Tile;

   ------------------
   -- Remove_Tiles --
   ------------------

   procedure Remove_Tiles
     (Tiles    : in out Surface_Tiles;
      Test     : not null access
        function (Tile : Carthage.Tiles.Tile_Type) return Boolean)
   is
      New_Tiles : Tile_Vectors.Vector;
   begin
      for Tile of Tiles.Tiles loop
         if not Test (Tile) then
            New_Tiles.Append (Tile);
         end if;
      end loop;
      Tiles.Tiles := New_Tiles;
   end Remove_Tiles;

   ------------------
   -- Remove_Tiles --
   ------------------

   procedure Remove_Tiles
     (Tiles        : in out Surface_Tiles;
      Position     : Tile_Position;
      Max_Distance : Natural)
   is
      Removed : array (1 .. Planet_Width * Planet_Height) of Boolean :=
                  (others => False);
      Rs      : Tile_Vectors.Vector;
      Start   : Positive := 1;
      Count   : Natural := 0;
   begin
      Rs.Append (Tiles.Planet.Tile (Position));
      Removed (Index_Of (Position)) := True;
      for D in 1 .. Max_Distance loop
         Count := Rs.Last_Index;
         for I in Start .. Count loop
            for N of Tiles.Planet.Neighbours (Rs.Element (I).Position) loop
               if not Removed (Index_Of (N)) then
                  Removed (Index_Of (N)) := True;
                  Rs.Append (Tiles.Planet.Tile (N));
               end if;
            end loop;
         end loop;
         Start := Count + 1;
      end loop;

      for R of Rs loop
         Remove_Tile (Tiles, R.Position);
      end loop;
   end Remove_Tiles;

   ---------------
   -- Road_Cost --
   ---------------

   function Road_Cost
     (Planet   : Planet_Record;
      Position : Tile_Position)
      return Natural
   is
      use type Carthage.Terrain.Terrain_Type;
   begin
      return Cost : Natural := 0 do
         for Layer in Carthage.Tiles.Terrain_Layer loop
            declare
               Terrain : constant Carthage.Terrain.Terrain_Type :=
                           Planet.Tile (Position).Terrain (Layer);
            begin
               if Terrain /= null then
                  Cost := Cost + Terrain.Road_Cost (Planet.Category_Name);
               end if;
            end;
         end loop;
      end return;
   end Road_Cost;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access procedure (Planet : Planet_Type))
   is
   begin
      Db.Scan (Process);
   end Scan;

   --------------------------
   -- Scan_Connected_Tiles --
   --------------------------

   procedure Scan_Connected_Tiles
     (Planet  : Planet_Record;
      Start   : Tile_Position;
      Test    : not null access
        function (Tile : Carthage.Tiles.Tile_Type) return Boolean;
      Process : not null access
        procedure (Tile : Carthage.Tiles.Tile_Type))
   is
      function Local_Test
        (Position : Tile_Position)
         return Boolean
      is (Test (Planet.Tile (Position)));

      procedure Local_Process (Position : Tile_Position);

      -------------------
      -- Local_Process --
      -------------------

      procedure Local_Process (Position : Tile_Position) is
      begin
         Process (Planet.Tile (Position));
      end Local_Process;

      Sub : Tile_Graphs.Sub_Graph;
   begin
      Surface_Graph.Connected_Sub_Graph
        (Start, Local_Test'Access, Sub);
      Tile_Graphs.Iterate (Sub, Local_Process'Access);
   end Scan_Connected_Tiles;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (Planet    : in out Planet_Record;
      New_Owner : Carthage.Houses.House_Type)
   is
   begin
      Planet.Owner := New_Owner;
   end Set_Owner;

   -----------------
   -- Set_Seen_By --
   -----------------

   procedure Set_Seen_By
     (Planet : Planet_Type;
      House  : Carthage.Houses.House_Type)
   is
      procedure Update (Rec : in out Planet_Class);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Planet_Class) is
      begin
         Carthage.Houses.Insert (Rec.Seen, House);
      end Update;

   begin
      Db.Update (Planet.Reference, Update'Access);
   end Set_Seen_By;

   -----------
   -- Stack --
   -----------

   function Stack
     (Planet : Planet_Record;
      House  : Carthage.Houses.House_Type)
      return access constant Carthage.Stacks.Stack_Record'Class
   is
   begin
      return Planet.Stacks.Element (House);
   end Stack;

   --------------
   -- Tile_Set --
   --------------

   function Tile_Set
     (Planet : Planet_Record)
      return String
   is (if Planet.Megacity
       then "megacity"
       elsif Planet.Category.Identifier = "desert"
       then "barren"
       else Planet.Category.Identifier);

   ------------
   -- Update --
   ------------

   procedure Update
     (Planet : Planet_Type;
      Update : not null access
        procedure (Planet : in out Planet_Class))
   is
   begin
      Db.Update (Planet.Reference, Update);
   end Update;

end Carthage.Planets;
