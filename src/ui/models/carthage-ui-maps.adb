with Ada.Containers.Doubly_Linked_Lists;

--  with WL.Images.FLC;
with WL.String_Maps;

--  with Carthage.Configure;

with Carthage.Cities;
with Carthage.Stacks;
with Carthage.Terrain;
with Carthage.Tiles;

package body Carthage.UI.Maps is

   type Direction is
     (North, Northeast, Southeast, South, Southwest, Northwest);

   function Direction_Of
     (From, To : Tile_Position) return Direction;

   type Direction_Flags is mod 2 ** 6;

   Powers : constant array (Direction) of Direction_Flags :=
              (1, 2, 4, 8, 16, 32);

   package Direction_Maps is
     new WL.String_Maps (Direction);

   Direction_Map : Direction_Maps.Map;

   function To_Direction
     (Name : String)
      return Direction
   is (Direction_Map.Element (Name));

   function To_Flag
     (Name : String)
      return Direction_Flags
   is (Powers (To_Direction (Name)));

   function To_Flags
     (Planet : Carthage.Planets.Planet_Type;
      Start  : Tile_Position;
      Test   : not null access
        function (Position : Tile_Position) return Boolean)
      return Direction_Flags;

   type Tile_Index is new Natural;

   type Tile_Index_Array is array (Direction_Flags) of Tile_Index;

   type Tile_Resource_Record is
      record
         Join     : Boolean := False;
         Base     : Tile_Index       := 0;
         Default  : Tile_Index       := 0;
         Always   : Tile_Index       := 0;
         Indices  : Tile_Index_Array := (others => 0);
      end record;

   type Tile_Resource_Access is access Tile_Resource_Record;

   package Tile_Resource_Maps is
     new WL.String_Maps (Tile_Resource_Access);

   Tile_Map : Tile_Resource_Maps.Map;

   package Terrain_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Terrain.Terrain_Type, Carthage.Terrain."=");

   Feature_Priority : Terrain_Lists.List;

   procedure Get_Terrain_Resources
     (Planet        : Carthage.Planets.Planet_Type;
      Tile          : Carthage.Tiles.Tile_Type;
      Layers        : in out Tile_Layers'Class);

   function Make_Resource_Name
     (Planet : Carthage.Planets.Planet_Type;
      Tile   : Tile_Index)
      return String
   is (Planet.Tile_Set & Integer'Image (-(Integer (Tile))) & "-tile");

   function Make_Background_Resource
     (Resource_Name : String;
      Colour        : Carthage.Colours.Colour_Type)
      return Layer_Element
   is (Resource_Name'Length, Background_Hex_Tile,
       Resource_Name, Colour);

   function Make_Hex_Tile_Resource
     (Resource_Name : String)
      return Layer_Element
   is (Resource_Name'Length, Hex_Tile, Resource_Name, (0.0, 0.0, 0.0, 0.0));

   function Make_Icon_Resource
     (Icon_Name : String;
      Background : Carthage.Colours.Colour_Type)
      return Layer_Element
   is (Icon_Name'Length, Icon, Icon_Name, Background);

--     procedure Check_Unit_Resource
--       (Renderer      : in out Lui.Rendering.Root_Renderer'Class;
--        Resource_Name : String)
--       with Unreferenced;

   -------------------------
   -- Check_Unit_Resource --
   -------------------------

--     procedure Check_Unit_Resource
--       (Renderer      : in out Lui.Rendering.Root_Renderer'Class;
--        Resource_Name : String)
--     is
--     begin
--        if not Renderer.Have_Resource (Resource_Name) then
--           declare
--              Reader : WL.Images.FLC.FLC_Image_Reader;
--              Image  : WL.Images.Image_Type;
--           begin
--              Reader.Read
--                (Carthage.Configure.Fading_Suns_FLC_File
--                   (Resource_Name),
--                 Image);
--              Renderer.Create_Image_Resource (Resource_Name, Image);
--           end;
--        end if;
--     end Check_Unit_Resource;

   ------------------------------
   -- Configure_Tile_Resources --
   ------------------------------

   procedure Configure_Tile_Resources
     (Config : Tropos.Configuration)
   is
      procedure Load_Terrain_Tiles
        (Terrain : Carthage.Terrain.Terrain_Type);

      ------------------------
      -- Load_Terrain_Tiles --
      ------------------------

      procedure Load_Terrain_Tiles
        (Terrain : Carthage.Terrain.Terrain_Type)
      is
         Name : constant String := Terrain.Identifier;
      begin
         if Config.Contains (Terrain.Identifier) then
            declare
               Tile_Config : constant Tropos.Configuration :=
                               Config.Child (Name);
               Item : constant Tile_Resource_Access :=
                               new Tile_Resource_Record;
            begin
               for Item_Config of Tile_Config loop
                  declare
                     Key : constant String := Item_Config.Config_Name;
                  begin
                     if Key = "default" then
                        Item.Default :=
                          Tile_Index (Natural'(Item_Config.Value));
                     elsif Key = "base" then
                        Item.Base :=
                          Tile_Index (Natural'(Item_Config.Value));
                     elsif Key = "all" then
                        Item.Indices (Direction_Flags'Last) :=
                          Tile_Index (Natural'(Item_Config.Value));
                     elsif Key = "none" then
                        Item.Indices (Direction_Flags'First) :=
                          Tile_Index (Natural'(Item_Config.Value));
                     elsif Key = "fade" then
                        null;
                     else
                        declare
                           Index : constant Tile_Index :=
                                     Tile_Index'Value (Key);
                           Flag  : Direction_Flags := 0;
                        begin
                           for D_Config of Item_Config loop
                              Flag := Flag +
                                To_Flag (D_Config.Config_Name);
                           end loop;
                           Item.Indices (Flag) := Index;
                        end;
                     end if;
                  end;
               end loop;

               Tile_Map.Insert (Name, Item);

            end;
         end if;
      end Load_Terrain_Tiles;

   begin
      for D in Direction loop
         Direction_Map.Insert (Direction'Image (D), D);
      end loop;

      Direction_Map.Insert ("n", North);
      Direction_Map.Insert ("ne", Northeast);
      Direction_Map.Insert ("nw", Northwest);
      Direction_Map.Insert ("s", South);
      Direction_Map.Insert ("se", Southeast);
      Direction_Map.Insert ("sw", Southwest);

      for Feature_Config of Config.Child ("feature-priority") loop
         Feature_Priority.Append
           (Carthage.Terrain.Get (Feature_Config.Config_Name));
      end loop;

      Carthage.Terrain.Scan
        (Load_Terrain_Tiles'Access);
   end Configure_Tile_Resources;

   ------------------
   -- Direction_Of --
   ------------------

   function Direction_Of
     (From, To : Tile_Position) return Direction
   is
   begin
      if From.X = To.X then
         if From.Y < To.Y then
            return South;
         else
            return North;
         end if;
      elsif From.X < To.X then
         if From.Y < To.Y
           or else (From.Y = To.Y and then From.X mod 2 = 0)
         then
            return Southeast;
         else
            return Northeast;
         end if;
      else
         if From.Y < To.Y
           or else (From.Y = To.Y and then From.X mod 2 = 0)
         then
            return Southwest;
         else
            return Northwest;
         end if;
      end if;
   end Direction_Of;

   ---------------------------
   -- Get_Terrain_Resources --
   ---------------------------

   procedure Get_Terrain_Resources
     (Planet        : Carthage.Planets.Planet_Type;
      Tile          : Carthage.Tiles.Tile_Type;
      Layers        : in out Tile_Layers'Class)
   is

      use Carthage.Terrain;
      Base_Layer : Carthage.Tiles.Terrain_Layer := 1;
      Feature_Layer : Carthage.Tiles.Terrain_Layer := 2;

      Base : Terrain_Type := Tile.Terrain (Base_Layer);
      Feature : Terrain_Type := Tile.Terrain (Feature_Layer);

      Feature_Index : Tile_Index;
      Base_Index    : Tile_Index;

      procedure Add
        (Index      : Tile_Index);

      ---------
      -- Add --
      ---------

      procedure Add
        (Index : Tile_Index)
      is
      begin
         if Index /= 0 then
            Layers.List.Append
              (Make_Hex_Tile_Resource
                 (Make_Resource_Name (Planet, Index)));
         end if;
      end Add;

   begin

      for Priority of Feature_Priority loop
         if Priority = Base then
            Base := Feature;
            Feature := Priority;
            Base_Layer := 2;
            Feature_Layer := 1;
            exit;
         end if;
         exit when Priority = Feature;
      end loop;

      if Base = null
        or else not Tile_Map.Contains (Base.Identifier)
      then
         Base_Index := 0;
      else
         declare
            Tile_Info : constant Tile_Resource_Access :=
                          Tile_Map.Element (Base.Identifier);

         begin
            Base_Index := Tile_Info.Base;
            Add (Base_Index);

            if Base_Index /= 0 then
               declare
                  Ns     : constant Carthage.Planets.Array_Of_Positions :=
                             Planet.Neighbours (Tile.Position);
               begin
                  for Neighbour of Ns loop
                     if Planet.Tile (Neighbour).Terrain (Feature_Layer)
                       /= Feature
                     then
                        Add (Base_Index + 1 +
                               Direction'Pos
                                 (Direction_Of
                                    (Tile.Position, Neighbour)));
                     end if;
                  end loop;
               end;
            end if;

         end;
      end if;

      if Feature = null
        or else not Tile_Map.Contains (Feature.Identifier)
      then
         Feature_Index := 0;
      else
         declare
            Tile_Info : constant Tile_Resource_Access :=
                          Tile_Map.Element (Feature.Identifier);

            function Same_Terrain
              (Neighbour : Tile_Position)
               return Boolean
            is (Planet.Tile (Neighbour).Terrain (Feature_Layer)
                = Feature);

            Flags : constant Direction_Flags :=
                      To_Flags (Planet, Tile.Position, Same_Terrain'Access);
         begin
            if Tile_Info.Indices (Flags) = 0 then
               Feature_Index := Tile_Info.Default;
            else
               Feature_Index := Tile_Info.Indices (Flags);
            end if;
         end;
      end if;

      Add (Feature_Index);

   end Get_Terrain_Resources;

   ---------------------
   -- Get_Tile_Layers --
   ---------------------

   procedure Get_Tile_Layers
     (Planet   : Carthage.Planets.Planet_Type;
      House    : Carthage.Houses.House_Type;
      Position : Tile_Position;
      Layers   : in out Tile_Layers'Class)
   is
      Tile : constant Carthage.Tiles.Tile_Type := Planet.Tile (Position);

      procedure Add
        (Index      : Tile_Index);

      ---------
      -- Add --
      ---------

      procedure Add
        (Index : Tile_Index)
      is
      begin
         if Index /= 0 then
            Layers.List.Append
              (Make_Hex_Tile_Resource
                 (Make_Resource_Name (Planet, Index)));
         end if;
      end Add;

   begin

      if not Tile.Seen_By (House) then
         return;
      end if;

      Get_Terrain_Resources (Planet, Tile, Layers);

      if Tile.Has_Road then

         --  Add (67);

         for Neighbour_Position of
           Planet.Neighbours (Position)
         loop
            if Planet.Tile (Neighbour_Position).Has_Road then
               case Direction_Of (Position, Neighbour_Position) is
                  when Northwest =>
                     Add (68);
                  when South =>
                     Add (69);
                  when Southeast =>
                     Add (70);
                  when North =>
                     Add (71);
                  when Southwest =>
                     Add (72);
                  when Northeast =>
                     Add (73);
               end case;
            end if;
         end loop;

      end if;

      if Tile.Has_City then

         Layers.List.Append
           (Make_Hex_Tile_Resource
              (Planet.Category.Identifier
               & "-"
               & Tile.City.Structure.Identifier
               & "-"
               & "hex"));

      end if;

      if Tile.Currently_Visible_To (House) then
         if Tile.Has_Stack then
            declare
               Background : Carthage.Colours.Colour_Type :=
                              Tile.Stack.Owner.Colour;
            begin
               Background.Alpha := 0.7;
               Layers.List.Append
                 (Make_Icon_Resource
                    ("unit"
                     & Integer'Image (-(Tile.Stack.Asset (1).Unit.Index)),
                     Background));
            end;
         end if;
      else
         Layers.List.Append
           (Make_Background_Resource
              ("shadow-hex-tile", (0.0, 0.0, 0.0, 0.4)));
      end if;

   end Get_Tile_Layers;

   -----------------
   -- Scan_Layers --
   -----------------

   procedure Scan_Layers
     (Layers  : Tile_Layers'Class;
      Process : not null access
        procedure (Element : Layer_Element_Type;
                   Resource_Name : String;
                   Color : Carthage.Colours.Colour_Type))
   is
   begin
      for Element of Layers.List loop
         Process (Element.Layer_Element,
                  Element.Resource_Name, Element.Color);
      end loop;
   end Scan_Layers;

   --------------
   -- To_Flags --
   --------------

   function To_Flags
     (Planet : Carthage.Planets.Planet_Type;
      Start  : Tile_Position;
      Test   : not null access
        function (Position : Tile_Position) return Boolean)
      return Direction_Flags
   is
      Ns : constant Carthage.Planets.Array_Of_Positions :=
             Planet.Neighbours (Start);
      Result : Direction_Flags := 0;
   begin
      for N of Ns loop
         if Test (N) then
            Result := Result + Powers (Direction_Of (Start, N));
         end if;
      end loop;
      return Result;
   end To_Flags;

end Carthage.UI.Maps;
