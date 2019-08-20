package body Carthage.Cities is

   ----------------------------
   -- Add_Scheduled_Transfer --
   ----------------------------

   procedure Add_Scheduled_Transfer
     (City     : in out City_Record;
      Item     : Carthage.Resources.Resource_Type;
      Quantity : Positive)
   is
   begin
      City.Stock.Remove (Item, Quantity);
      City.Scheduled.Add (Item, Quantity);
   end Add_Scheduled_Transfer;

   procedure After_Agora_Transaction
     (City            : in out City_Record;
      Resource        : Carthage.Resources.Resource_Type;
      Quantity_Change : Integer)
   is
      Price   : Agora_Resource_Record renames
        City.Prices (Resource.Index);
   begin
      if Quantity_Change /= 0 then
         if Quantity_Change > 0 then
            Price.Buy_Price := Natural'Max (Price.Buy_Price - 1, 1);
            Price.Sell_Price :=
              Natural'Max (Price.Sell_Price - 1, Price.Buy_Price + 1);
         else
            Price.Buy_Price := Price.Buy_Price + 1;
            Price.Sell_Price :=
              Natural'Max (Price.Sell_Price + 1, Price.Buy_Price + 1);
         end if;
         City.Log
           ("price of "
            & Resource.Identifier
            & " after "
            & (if Quantity_Change > 0 then "adding" else "removing")
            & Natural'Image (abs Quantity_Change)
            & " now"
            & Price.Buy_Price'Image
            & " buy,"
            & Price.Sell_Price'Image
            & " sell");
      end if;
   end After_Agora_Transaction;

   --------------------
   -- Agora_Buys_For --
   --------------------

   function Agora_Buys_For
     (City     : City_Record;
      Resource : Carthage.Resources.Resource_Type)
      return Positive
   is (City.Prices.Element (Resource.Index).Buy_Price);

   ---------------------
   -- Agora_Sells_For --
   ---------------------

   function Agora_Sells_For
     (City     : City_Record;
      Resource : Carthage.Resources.Resource_Type)
      return Positive
   is (City.Prices.Element (Resource.Index).Sell_Price);

   ------------------
   -- Buy_Resource --
   ------------------

   procedure Buy_Resource
     (City     : in out City_Record;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : Positive)
   is
   begin
      if City.Agora /= null then
         City.Log ("buy" & Quantity'Image & " " & Resource.Name);
         City.Orders.Append
           (City_Order_Record'
              (Buy, City.Agora, Resource, Quantity));
      end if;
   end Buy_Resource;

   --------------------
   -- Log_Identifier --
   --------------------

   overriding function Log_Identifier
     (City : City_Record)
      return String
   is
   begin
      return City.Owner.Identifier & " " & City_Class (City).Identifier;
   end Log_Identifier;

   -------------------------------
   -- Remove_Scheduled_Transfer --
   -------------------------------

   procedure Remove_Scheduled_Transfer
     (City  : in out City_Record;
      Stock : Carthage.Resources.Stock_Interface'Class)
   is
   begin
      City.Scheduled.Remove_Stock (Stock);
   end Remove_Scheduled_Transfer;

   -----------------
   -- Scan_Cities --
   -----------------

   procedure Scan_Cities
     (Process : not null access procedure (City : City_Type))
   is
   begin
      Db.Scan (Process);
   end Scan_Cities;

   -----------------
   -- Scan_Cities --
   -----------------

   procedure Scan_Cities
     (Test    : not null access function (City : City_Type) return Boolean;
      Process : not null access procedure (City : City_Type))
   is
   begin
      Db.Scan (Test, Process);
   end Scan_Cities;

   -----------------
   -- Scan_Cities --
   -----------------

   procedure Scan_Cities
     (Structure : Carthage.Structures.Structure_Type;
      Process   : not null access procedure (City : City_Type))
   is
      use type Carthage.Structures.Structure_Type;

      function OK (City : City_Type) return Boolean
      is (City.Structure = Structure);

   begin
      Scan_Cities (OK'Access, Process);
   end Scan_Cities;

   -----------------
   -- Scan_Cities --
   -----------------

   procedure Scan_Cities
     (Owner   : Carthage.Houses.House_Type;
      Process : not null access procedure (City : City_Type))
   is
      use type Carthage.Houses.House_Type;

      function OK (City : City_Type) return Boolean
      is (City.Owner = Owner);

   begin
      Scan_Cities (OK'Access, Process);
   end Scan_Cities;

   ------------------------
   -- Scan_Planet_Cities --
   ------------------------

   procedure Scan_Planet_Cities
     (Planet  : Carthage.Planets.Planet_Type;
      Process : not null access
        procedure (City : City_Type))
   is
      use Carthage.Planets;

      function Same_Planet (City : City_Type) return Boolean
      is (City.Planet = Planet);

   begin
      Db.Scan (Same_Planet'Access, Process);
   end Scan_Planet_Cities;

   -------------------
   -- Sell_Resource --
   -------------------

   procedure Sell_Resource
     (City     : in out City_Record;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : Positive)
   is
   begin
      if City.Agora /= null then
         City.Orders.Append
           (City_Order_Record'(Sell, City.Agora, Resource, Quantity));
      end if;
   end Sell_Resource;

   ---------------
   -- Set_Agora --
   ---------------

   procedure Set_Agora
     (City  : in out City_Record;
      Agora : not null access constant City_Record'Class)
   is
   begin
      City.Agora := City_Type (Agora);
   end Set_Agora;

   -----------------
   -- Set_Manager --
   -----------------

   procedure Set_Manager
     (City    : in out City_Record;
      Manager : not null access City_Manager_Interface'Class)
   is
   begin
      City.Manager := Manager;
   end Set_Manager;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (City         : in out City_Record;
      Resource     : not null access constant
        Carthage.Resources.Resource_Class;
      New_Quantity : Resource_Quantity)
   is
   begin
      City.Stock.Set_Quantity (Resource, New_Quantity);
   end Set_Quantity;

   -----------------
   -- Set_Seen_By --
   -----------------

   procedure Set_Seen_By
     (City  : City_Type;
      House : Carthage.Houses.House_Type)
   is
      procedure Update (Rec : in out City_Class);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out City_Class) is
      begin
         Carthage.Houses.Insert (Rec.Seen, House);
      end Update;

   begin
      Db.Update (City.Reference, Update'Access);
   end Set_Seen_By;

   -----------------------
   -- Transfer_Resource --
   -----------------------

   procedure Transfer_Resource
     (City     : in out City_Record;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : Positive;
      To_City  : not null access constant City_Record'Class)
   is
   begin
      City.Remove (Resource, Quantity);
      To_City.Update.Add (Resource, Quantity);
--        City.Orders.Append
--          (City_Order_Record'
--             (Transfer, City_Type (To_City),
--              Resource, Quantity));
   end Transfer_Resource;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant City_Record'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   -----------------
   -- Update_City --
   -----------------

   procedure Update_City
     (City   : City_Type;
      Update : not null access
        procedure (City : in out City_Class))
   is
   begin
      Db.Update (City.Reference, Update);
   end Update_City;

end Carthage.Cities;
