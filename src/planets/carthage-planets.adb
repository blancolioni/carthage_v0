with Carthage.Stacks;
with Carthage.Terrain;

package body Carthage.Planets is

   --------------
   -- Add_City --
   --------------

   procedure Add_City
     (Planet : in out Planet_Record;
      City   : not null access constant Carthage.Cities.City_Record'Class)
   is
   begin
      Planet.Cities.Append (Planet_City_Access (City));
   end Add_City;

   ----------------------
   -- Clear_Visibility --
   ----------------------

   procedure Clear_Visibility (Planet : in out Planet_Class) is
      procedure Clear_Tile_Visibility
        (Tile : Carthage.Tiles.Tile_Type);

      ---------------------------
      -- Clear_Tile_Visibility --
      ---------------------------

      procedure Clear_Tile_Visibility
        (Tile : Carthage.Tiles.Tile_Type)
      is
      begin
         Tile.Update.Clear_Visibility;
      end Clear_Tile_Visibility;

   begin
      Tile_Grids.Scan_Tiles (Planet.Grid, Clear_Tile_Visibility'Access);
      Carthage.Houses.Clear (Planet.Seen);
      Carthage.Houses.Clear (Planet.Explored);
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

      function Passable (Tile : Carthage.Tiles.Tile_Type) return Boolean
      is (Cost (Tile) /= 0.0);

      Path : constant Hexes.Cube_Coordinate_Array :=
               Planet.Grid.Find_Path
                 (Start    => Planet.To_Cubic (Start),
                  Finish   => Planet.To_Cubic (Finish),
                  Passable => Passable'Access,
                  Cost     => Cost);
   begin
      return Result : Array_Of_Positions (Path'Range) do
         for I in Result'Range loop
            Result (I) := Planet.To_Position (Path (I));
         end loop;
      end return;
   end Find_Path;

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

   procedure Get_Tiles
     (Planet : not null access constant Planet_Record'Class;
      Test   : not null access
        function (Tile : Carthage.Tiles.Tile_Type)
      return Boolean;
      Tiles  : out Surface_Tiles)
   is
      procedure Check (Tile : Carthage.Tiles.Tile_Type);

      procedure Check (Tile : Carthage.Tiles.Tile_Type) is
      begin
         if Test (Tile) then
            Tiles.Tiles.Append (Tile);
         end if;
      end Check;

   begin

      Tiles.Planet := Planet_Type (Planet);
      Planet.Grid.Scan_Tiles (Check'Access);
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
               if Current_D in Min_Distance .. Max_Distance then
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
      function Is_Land (Tile : Carthage.Tiles.Tile_Type) return Boolean
      is (not Tile.Is_Water);

      function Constant_Cost (Tile : Carthage.Tiles.Tile_Type) return Float
      is (1.0);

      Path : constant Hexes.Cube_Coordinate_Array :=
               Planet.Grid.Find_Path
                 (Start    => Planet.To_Cubic (From),
                  Finish   => Planet.To_Cubic (To),
                  Passable => Is_Land'Access,
                  Cost     => Constant_Cost'Access);
   begin
      return From = To or else Path'Length > 1;
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
      Cubics : constant Hexes.Cube_Coordinate_Array :=
                 Planet.Grid.Neighbours
                   (Planet.To_Cubic (Position));
   begin
      return Result : Array_Of_Positions (Cubics'Range) do
         for I in Result'Range loop
            Result (I) := Planet.To_Position (Cubics (I));
         end loop;
      end return;
   end Neighbours;

   -----------------
   -- Remove_City --
   -----------------

   procedure Remove_City
     (Planet : in out Planet_Record;
      City   : not null access constant Carthage.Cities.City_Record'Class)
   is
      Position : Planet_City_Lists.Cursor :=
                   Planet.Cities.Find (Planet_City_Access (City));
   begin
      Planet.Cities.Delete (Position);
   end Remove_City;

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
      Removed (Tile_Position_Index (Position)) := True;
      for D in 1 .. Max_Distance loop
         Count := Rs.Last_Index;
         for I in Start .. Count loop
            for N of Tiles.Planet.Neighbours (Rs.Element (I).Position) loop
               if not Removed (Tile_Position_Index (N)) then
                  Removed (Tile_Position_Index (N)) := True;
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

   -----------------
   -- Scan_Cities --
   -----------------

   procedure Scan_Cities
     (Planet  : Planet_Record;
      Process : not null access
        procedure (City : not null access constant
                     Carthage.Cities.City_Record'Class))
   is
   begin
      for City of Planet.Cities loop
         Process (City);
      end loop;
   end Scan_Cities;

   procedure Scan_Connected_Tiles
     (Planet  : Planet_Record;
      Start   : Tile_Position;
      Test    : not null access
        function (Tile : Carthage.Tiles.Tile_Type) return Boolean;
      Process : not null access
        procedure (Tile : Carthage.Tiles.Tile_Type))
   is
   begin
      null;
   end Scan_Connected_Tiles;

   ----------------------------
   -- Scan_Neighbours_Within --
   ----------------------------

   procedure Scan_Neighbours_Within
     (Planet   : Planet_Record;
      Start    : Tile_Position;
      Distance : Natural;
      Process  : not null access
        procedure (Tile : Carthage.Tiles.Tile_Type))
   is
      Ns : Surface_Tiles;
   begin
      Planet.Get_Tiles (Planet.Tile (Start), 1, Distance, null, Ns);
      for I in 1 .. Tile_Count (Ns) loop
         Process (Get_Tile (Ns, I));
      end loop;
   end Scan_Neighbours_Within;

   -----------------
   -- Scan_Stacks --
   -----------------

   procedure Scan_Stacks
     (Planet  : Planet_Record;
      Process : not null access
        procedure (Stack : not null access constant
                     Carthage.Stacks.Stack_Record'Class))
   is
   begin
      Planet.Scan_Stacks (null, Process);
   end Scan_Stacks;

   -----------------
   -- Scan_Stacks --
   -----------------

   procedure Scan_Stacks
     (Planet  : Planet_Record;
      Owner   : Carthage.Houses.House_Type;
      Process : not null access
        procedure (Stack : not null access constant
                     Carthage.Stacks.Stack_Record'Class))
   is
      procedure Check (Stack : Carthage.Stacks.Stack_Type);

      -----------
      -- Check --
      -----------

      procedure Check (Stack : Carthage.Stacks.Stack_Type) is
         use type Carthage.Houses.House_Type;
      begin
         if Stack.Planet.Identifier = Planet.Identifier
           and then (Owner = null or else Stack.Owner = Owner)
         then
            Process (Stack);
         end if;
      end Check;

   begin
      Carthage.Stacks.Scan_Stacks (Check'Access);
   end Scan_Stacks;

   ---------------------
   -- Set_Explored_By --
   ---------------------

   procedure Set_Explored_By
     (Planet : in out Planet_Record;
      House  : Carthage.Houses.House_Type)
   is
   begin
      Carthage.Houses.Insert (Planet.Explored, House);
   end Set_Explored_By;

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
     (Planet : in out Planet_Record;
      House  : Carthage.Houses.House_Type)
   is
   begin
      Carthage.Houses.Insert (Planet.Seen, House);
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
       then "city"
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

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Planet_Record'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Carthage.Planets;
