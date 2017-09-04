package body Carthage.Cities is

   ------------------
   -- Buy_Resource --
   ------------------

   procedure Buy_Resource
     (City     : in out City_Record;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : Positive)
   is
   begin
      City.Orders.Append (City_Order_Record'(Buy, Resource, Quantity));
   end Buy_Resource;

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
      City.Orders.Append (City_Order_Record'(Sell, Resource, Quantity));
   end Sell_Resource;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (City         : in out City_Record;
      Resource     : not null access constant
        Carthage.Resources.Resource_Class;
      New_Quantity : Natural)
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
