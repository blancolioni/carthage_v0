package body Carthage.Cities is

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

end Carthage.Cities;
