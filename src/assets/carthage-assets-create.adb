package body Carthage.Assets.Create is

   Current_Asset_Id : String := "A0-AA-A0A0";

   function Next_Asset_Id return String;

   function New_Asset
     (Unit      : Carthage.Units.Unit_Type;
      Owner     : Carthage.Houses.House_Type;
      XP        : Asset_Experience := Green;
      Loyalty   : Asset_Loyalty := Asset_Loyalty'Last;
      Health    : Asset_Health := Asset_Health'Last)
      return Asset_Type
   is
      procedure Create (Asset : in out Asset_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Asset : in out Asset_Class) is
      begin
         Asset.Create_With_Identity
           (Next_Asset_Id & "-" & Owner.Identifier & "-" & Unit.Identifier);
         Asset.Set_Name (Unit.Identifier);
         Asset.Owner := Owner;
         Asset.Unit := Unit;
         Asset.Experience := XP;
         Asset.Loyalty := Loyalty;
         Asset.Health := Health;
      end Create;

      Asset : constant Asset_Type :=
               Db.Create (Create'Access);
   begin
      return Asset;
   end New_Asset;

   -------------------
   -- Next_Asset_Id --
   -------------------

   function Next_Asset_Id return String is
   begin
      for Ch of reverse Current_Asset_Id loop
         if Ch = '9' then
            Ch := '0';
         elsif Ch = 'Z' then
            Ch := 'A';
         elsif Ch in '0' .. '8'
           or else Ch in 'A' .. 'Z'
         then
            Ch := Character'Succ (Ch);
            exit;
         end if;
      end loop;
      return Current_Asset_Id;
   end Next_Asset_Id;

end Carthage.Assets.Create;
