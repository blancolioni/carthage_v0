with Ada.Containers.Doubly_Linked_Lists;

with WL.String_Maps;

with Carthage.Calendar;
with Carthage.Planets;
with Carthage.Resources;
with Carthage.Structures;

with Carthage.Cities.Updates;

with Carthage.Managers.Assets;

package body Carthage.Managers.Cities is

   type City_Resource is
      record
         City     : Carthage.Cities.City_Type;
         Resource : Carthage.Resources.Resource_Type;
      end record;

   package City_Resource_Lists is
     new Ada.Containers.Doubly_Linked_Lists (City_Resource);

   package City_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Cities.City_Type, Carthage.Cities."=");

   package Resource_Quantity_Vectors is
     new Ada.Containers.Vectors
       (Carthage.Resources.Resource_Index, Resource_Quantity);

   package City_Request_Maps is
     new WL.String_Maps
       (Resource_Quantity_Vectors.Vector, Resource_Quantity_Vectors."=");

   type City_Trade_Group_Record is
      record
         City_Requests : City_Request_Maps.Map;
      end record;

   procedure Set_Request
     (Group    : City_Trade_Group;
      City     : Carthage.Cities.City_Type;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : Resource_Quantity);

   function Get_Total_Requests
     (Group    : City_Trade_Group;
      Resource : Carthage.Resources.Resource_Type)
      return Resource_Quantity;

   procedure Scan_Requests
     (Group    : City_Trade_Group;
      Resource : Carthage.Resources.Resource_Type;
      Process  : not null access
        procedure (City : Carthage.Cities.City_Type;
                   Quantity : in out Resource_Quantity));

   type City_Manager_Record is
     new Root_Manager_Type
     and Carthage.Cities.City_Manager_Interface with
      record
         Planet         : Carthage.Planets.Planet_Type;
         Group          : City_Trade_Group;
         City           : Carthage.Cities.City_Type;
         Palace         : Carthage.Cities.City_Type;
         Shield         : Carthage.Cities.City_Type;
         Agoras         : City_Lists.List;
         Available      : Carthage.Resources.Stock_Record;
         Ordered        : Carthage.Resources.Stock_Record;
         Scheduled      : Carthage.Resources.Stock_Record;
         Sources        : City_Resource_Lists.List;
         Sinks          : City_Resource_Lists.List;
         City_Requests  : City_Request_Maps.Map;
         Ground_Manager : Manager_Type;
      end record;

   type City_Manager_Type is access all City_Manager_Record'Class;

   overriding procedure On_Hostile_Spotted
     (Manager : in out City_Manager_Record;
      Spotter : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class)
   is null;

   overriding function Average_Update_Frequency
     (Manager : City_Manager_Record)
      return Duration
   is (Carthage.Calendar.Days (1));

   overriding procedure Initialize
     (Manager : not null access City_Manager_Record);

   overriding function Update
     (Manager : not null access City_Manager_Record)
      return Duration;

   procedure Create_Resource_Network
     (Manager : in out City_Manager_Record'Class);

   procedure Create_Transfer_Goals
     (Manager   : in out City_Manager_Record'Class;
      Process   : not null access
        procedure (To_City : Carthage.Cities.City_Type;
                   Stock   : Carthage.Resources.Stock_Record));

   -------------------------
   -- Create_City_Manager --
   -------------------------

   function Create_City_Manager
     (House  : Carthage.Houses.House_Type;
      Group  : City_Trade_Group;
      City   : not null access constant Carthage.Cities.City_Record'Class;
      Ground : Manager_Type)
      return Manager_Type
   is
      Manager : constant City_Manager_Type :=
                  new City_Manager_Record;
   begin
      Manager.House := House;
      Manager.Planet := City.Planet;
      Manager.Group := Group;
      Manager.Ground_Manager := Ground;

      declare
         V : Resource_Quantity_Vectors.Vector;
      begin
         for I in 1 .. Carthage.Resources.Last_Index loop
            V.Append (0.0);
         end loop;
         Group.City_Requests.Insert (City.Identifier, V);
      end;

      Manager.City := Carthage.Cities.City_Type (City);
      Manager.Initialize;
      Add_Manager (Manager);
      return Manager_Type (Manager);
   end Create_City_Manager;

   -----------------------------
   -- Create_Resource_Network --
   -----------------------------

   procedure Create_Resource_Network
     (Manager : in out City_Manager_Record'Class)
   is
      use Carthage.Cities, Carthage.Structures;
      City      : constant City_Type := Manager.City;
      Structure : constant Structure_Type := City.Structure;
      Inputs    : constant Production_Array :=
                    Structure.Production_Inputs;
      Outputs   : constant Production_Array :=
                    Structure.Production_Outputs;

      procedure Add_Connection
        (Connected_City : not null access constant City_Record'Class);

      --------------------
      -- Add_Connection --
      --------------------

      procedure Add_Connection
        (Connected_City : not null access constant City_Record'Class)
      is
      begin
         if Connected_City.Structure.Is_Palace then
            Manager.Palace := City_Type (Connected_City);
         elsif Connected_City.Structure.Is_Shield then
            Manager.Shield := City_Type (Connected_City);
         elsif Connected_City.Structure.Is_Agora then
            Manager.Agoras.Append (City_Type (Connected_City));
         end if;

         for Item of Inputs loop
            if Connected_City.Structure.Produces (Item.Resource) then
               Manager.Sources.Append
                 (City_Resource'
                    (City     => City_Type (Connected_City),
                     Resource => Item.Resource));
            end if;
         end loop;

         for Item of Outputs loop
            if Connected_City.Structure.Consumes (Item.Resource) then
               Manager.Sinks.Append
                 (City_Resource'
                    (City     => City_Type (Connected_City),
                     Resource => Item.Resource));
            end if;
         end loop;
      end Add_Connection;

   begin
      City.Log ("creating resource network");

      Manager.Planet.Scan_Cities (Manager.House, Add_Connection'Access);

   end Create_Resource_Network;

   ---------------------------
   -- Create_Transfer_Goals --
   ---------------------------

   procedure Create_Transfer_Goals
     (Manager   : in out City_Manager_Record'Class;
      Process   : not null access
        procedure (To_City : Carthage.Cities.City_Type;
                   Stock   : Carthage.Resources.Stock_Record))
   is
      type City_Transfer is
         record
            City : Carthage.Cities.City_Type;
            Stock : Carthage.Resources.Stock_Record;
         end record;

      package City_Transfer_Lists is
        new Ada.Containers.Doubly_Linked_Lists (City_Transfer);

      Transfers : City_Transfer_Lists.List;

      procedure Add_Transfer
        (To_City  : Carthage.Cities.City_Type;
         Resource : Carthage.Resources.Resource_Type;
         Quantity : Natural);

      ------------------
      -- Add_Transfer --
      ------------------

      procedure Add_Transfer
        (To_City  : Carthage.Cities.City_Type;
         Resource : Carthage.Resources.Resource_Type;
         Quantity : Natural)
      is
         use type Carthage.Cities.City_Type;
      begin
         for Transfer of Transfers loop
            if Transfer.City = To_City then
               Transfer.Stock.Add (Resource, Quantity);
               return;
            end if;
         end loop;

         declare
            Stock : Carthage.Resources.Stock_Record;
         begin
            Stock.Add (Resource, Quantity);
            Transfers.Append
              (City_Transfer'
                 (City  => To_City,
                  Stock => Stock));
         end;
      end Add_Transfer;

   begin
      for Item of Manager.City.Structure.Production_Outputs loop
         declare
            Total_Requests : constant Resource_Quantity :=
              Get_Total_Requests (Manager.Group, Item.Resource);
            Available      : constant Resource_Quantity :=
              Manager.Available.Quantity (Item.Resource);
            Factor         : constant Float :=
              (if Available >= Total_Requests
               then 1.0
               else Float (Available)
               / Float (Total_Requests));

            procedure Process_Request
              (To_City  : Carthage.Cities.City_Type;
               Quantity : in out Resource_Quantity);

            ---------------------
            -- Process_Request --
            ---------------------

            procedure Process_Request
              (To_City  : Carthage.Cities.City_Type;
               Quantity : in out Resource_Quantity)
            is
            begin
               if Factor < 1.0 then
                  Quantity := Resource_Quantity (Factor * Float (Quantity));
               end if;

               declare
                  Whole_Quantity : constant Natural :=
                    Natural
                      (Float'Truncation (Float (Quantity)));
               begin
                  if Whole_Quantity > 0 then
                     Manager.City.Log
                       ("new goal:"
                        & Natural'Image (Natural (Quantity))
                        & " " & Item.Resource.Name
                        & " to " & To_City.Identifier);

                     Add_Transfer (To_City, Item.Resource, Whole_Quantity);

                     Manager.Available.Remove (Item.Resource, Whole_Quantity);
                     Manager.Scheduled.Add (Item.Resource, Whole_Quantity);

                  end if;
               end;
            end Process_Request;

         begin
            Scan_Requests (Manager.Group,
                           Item.Resource, Process_Request'Access);
         end;
      end loop;

      for Transfer of Transfers loop
         Process (Transfer.City, Transfer.Stock);
      end loop;

   end Create_Transfer_Goals;

   ------------------------
   -- Get_Total_Requests --
   ------------------------

   function Get_Total_Requests
     (Group    : City_Trade_Group;
      Resource : Carthage.Resources.Resource_Type)
      return Resource_Quantity
   is
   begin
      return Result : Resource_Quantity := 0.0 do
         for Reqs of Group.City_Requests loop
            Result := Result + Reqs.Element (Resource.Index);
         end loop;
      end return;
   end Get_Total_Requests;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : not null access City_Manager_Record)
   is
   begin
      Manager.Create_Resource_Network;
   end Initialize;

   ---------------------
   -- New_Trade_Group --
   ---------------------

   function New_Trade_Group return City_Trade_Group is
   begin
      return new City_Trade_Group_Record;
   end New_Trade_Group;

   -------------------
   -- Scan_Requests --
   -------------------

   procedure Scan_Requests
     (Group    : City_Trade_Group;
      Resource : Carthage.Resources.Resource_Type;
      Process  : not null access
        procedure (City : Carthage.Cities.City_Type;
                   Quantity : in out Resource_Quantity))
   is
      use City_Request_Maps;
      Position : Cursor := Group.City_Requests.First;
   begin
      while Has_Element (Position) loop
         if Element (Position).Element (Resource.Index) > 0.0 then
            declare
               Quantity : Resource_Quantity :=
                            Element (Position).Element (Resource.Index);
            begin
               Process (Carthage.Cities.Get (Key (Position)), Quantity);
               Group.City_Requests (Position).Replace_Element
                 (Resource.Index, Quantity);
            end;
         end if;
         Next (Position);
      end loop;
   end Scan_Requests;

   -----------------
   -- Set_Request --
   -----------------

   procedure Set_Request
     (Group    : City_Trade_Group;
      City     : Carthage.Cities.City_Type;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : Resource_Quantity)
   is
   begin
      Group.City_Requests (City.Identifier).Replace_Element
        (Resource.Index, Quantity);
   end Set_Request;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Manager : not null access City_Manager_Record)
      return Duration
   is
      use Carthage.Cities, Carthage.Structures;
      use Carthage.Resources;

      City      : constant City_Type := Manager.City;
      Structure : constant Structure_Type := City.Structure;

      procedure Save_Quantity
        (Resource : Carthage.Resources.Resource_Type;
         Quantity : Positive);

      -------------------
      -- Save_Quantity --
      -------------------

      procedure Save_Quantity
        (Resource : Carthage.Resources.Resource_Type;
         Quantity : Positive)
      is
      begin
         Manager.Available.Set_Quantity
           (Resource, Resource_Quantity (Quantity));
      end Save_Quantity;

   begin

      Manager.Available.Clear_Stock;
      Manager.City.Scan_Stock (Save_Quantity'Access);

      if not Structure.Is_Harvester then

         for Item of Structure.Production_Inputs loop
            declare
               Have : constant Resource_Quantity :=
                        City.Quantity (Item.Resource);
               Want : constant Resource_Quantity :=
                        2.0 * Item.Quantity;
            begin
               Set_Request
                 (Manager.Group, City, Item.Resource,
                  (if Have >= Want then 0.0 else Want - Have));
            end;
         end loop;
      end if;

      declare
         procedure Create_Goals
           (To_City : Carthage.Cities.City_Type;
            Stock   : Carthage.Resources.Stock_Record);

         ------------------
         -- Create_Goals --
         ------------------

         procedure Create_Goals
           (To_City : Carthage.Cities.City_Type;
            Stock   : Carthage.Resources.Stock_Record)
         is
         begin
            Manager.City.Log
              ("new goal: transfer resources to "
               & To_City.Identifier);

            declare
               procedure Show
                 (Resource : Carthage.Resources.Resource_Type;
                  Quantity : Positive);

               ----------
               -- Show --
               ----------

               procedure Show
                 (Resource : Carthage.Resources.Resource_Type;
                  Quantity : Positive)
               is
               begin
                  Manager.City.Log
                    (Resource.Identifier & ":" & Quantity'Img);
               end Show;

            begin
               Stock.Scan_Stock
                 (Show'Access);
            end;

            Manager.Ground_Manager.Add_Goal
              (Carthage.Managers.Assets.Transfer_Cargo_Goal
                 (From     => City,
                  To       => To_City,
                  Cargo    => Stock));
         end Create_Goals;

      begin
         Manager.Create_Transfer_Goals (Create_Goals'Access);
      end;

      declare
         procedure Buy
           (Resource : Carthage.Resources.Resource_Type;
            Quantity : Positive);

         procedure Sell
           (Resource : Carthage.Resources.Resource_Type;
            Quantity : Positive);

         procedure Store
           (Resource : Carthage.Resources.Resource_Type;
            Quantity : Positive);

         ---------
         -- Buy --
         ---------

         procedure Buy
           (Resource : Carthage.Resources.Resource_Type;
            Quantity : Positive)
         is
         begin
            City.Log
              ("order" & Quantity'Img & " " & Resource.Identifier);
            City.Update.Buy_Resource (Resource, Quantity);
            Manager.Ordered.Add (Resource, Quantity);
         end Buy;

         ----------
         -- Sell --
         ----------

         procedure Sell
           (Resource : Carthage.Resources.Resource_Type;
            Quantity : Positive)
         is
         begin
            City.Log
              ("sell" & Quantity'Img & " " & Resource.Identifier);
            City.Update.Sell_Resource (Resource, Quantity);
         end Sell;

         -----------
         -- Store --
         -----------

         procedure Store
           (Resource : Carthage.Resources.Resource_Type;
            Quantity : Positive)
         is
         begin
            City.Log
              ("store" & Quantity'Img & " " & Resource.Identifier);
            City.Update.Transfer_Resource
              (Resource, Quantity, Manager.Palace);
         end Store;

      begin
         if City.Structure.Is_Harvester then
            if Manager.Palace /= null then
               Manager.Available.Scan_Stock (Store'Access);
            else
               Manager.Available.Scan_Stock (Sell'Access);
            end if;
         else
            declare
               Inputs  : constant Carthage.Structures.Production_Array :=
                           City.Structure.Production_Inputs;
               Outputs : constant Carthage.Structures.Production_Array :=
                           City.Structure.Production_Outputs;
            begin
               for Item of Inputs loop
                  declare
                     Available : constant Natural :=
                                   Manager.Available.Whole_Quantity
                                     (Item.Resource);
                     Ordered   : constant Natural :=
                                   Manager.Ordered.Whole_Quantity
                                     (Item.Resource);
                     Pipeline  : constant Natural :=
                                   Available + Ordered;
                     Required  : constant Natural :=
                                   2 * Natural (Item.Quantity);
                  begin
                     if Pipeline < Required then
                        Buy (Item.Resource, Required - Pipeline);
                     end if;
                  end;
               end loop;

               for Item of Outputs loop
                  if Manager.Available.Quantity (Item.Resource) > 0.0 then
                     if Manager.Palace /= null then
                        Store (Item.Resource,
                               Manager.Available.Whole_Quantity
                                 (Item.Resource));
                     else
                        Sell (Item.Resource,
                              Manager.Available.Whole_Quantity
                                (Item.Resource));
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end;

      if City.Structure.Has_Production then
         City.Log ("executing production");
         Carthage.Cities.Update_City
           (Manager.City,
            Carthage.Cities.Updates.Execute_City_Production'Access);
      end if;

      return Manager.Average_Update_Frequency;
   end Update;

end Carthage.Managers.Cities;
