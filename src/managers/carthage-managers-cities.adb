with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Resources;
with Carthage.Structures;

package body Carthage.Managers.Cities is

   -------------------------
   -- Create_City_Manager --
   -------------------------

   function Create_City_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return City_Manager_Type
   is
      Manager : City_Manager_Record;
   begin
      Manager.House := House;
      Manager.Planet := Planet;
      return new City_Manager_Record'(Manager);
   end Create_City_Manager;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Manager : in out City_Manager_Record)
   is
   begin
      for Info of Manager.Cities loop
         declare
            procedure Buy
              (Resource : Carthage.Resources.Resource_Type;
               Quantity : Natural);

            procedure Sell
              (Resource : Carthage.Resources.Resource_Type;
               Quantity : Natural);

            ---------
            -- Buy --
            ---------

            procedure Buy
              (Resource : Carthage.Resources.Resource_Type;
               Quantity : Natural)
            is
            begin
               Info.City.Log
                 ("order" & Quantity'Img & " " & Resource.Identifier);
               Info.City.Update.Buy_Resource (Resource, Quantity);
            end Buy;

            ----------
            -- Sell --
            ----------

            procedure Sell
              (Resource : Carthage.Resources.Resource_Type;
               Quantity : Natural)
            is
            begin
               Info.City.Log
                 ("sell" & Quantity'Img & " " & Resource.Identifier);
               Info.City.Update.Sell_Resource (Resource, Quantity);
            end Sell;

         begin
            if Info.City.Structure.Is_Harvester then
               Info.City.Scan_Stock (Sell'Access);
            else
               declare
                  Inputs  : constant Carthage.Structures.Production_Array :=
                              Info.City.Structure.Production_Inputs;
                  Outputs : constant Carthage.Structures.Production_Array :=
                              Info.City.Structure.Production_Outputs;
               begin
                  for Item of Inputs loop
                     if Info.City.Quantity (Item.Resource)
                       < 2 * Item.Quantity
                     then
                        Buy (Item.Resource,
                             2 * Item.Quantity
                             - Info.City.Quantity (Item.Resource));
                     end if;
                  end loop;

                  for Item of Outputs loop
                     if Info.City.Quantity (Item.Resource) > 0 then
                        Sell
                          (Item.Resource, Info.City.Quantity (Item.Resource));
                     end if;
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Execute;

   ------------------------
   -- Load_Initial_State --
   ------------------------

   overriding procedure Load_Initial_State
     (Manager : in out City_Manager_Record)
   is

      procedure Add_City_Info
        (City : not null access constant Carthage.Cities.City_Class);

      -------------------
      -- Add_City_Info --
      -------------------

      procedure Add_City_Info
        (City : not null access constant Carthage.Cities.City_Class)
      is
         use type Carthage.Houses.House_Type;
      begin
         if City.Owner = Manager.House then
            Manager.Cities.Append
              (City_Info_Record'
                 (City =>
                      Carthage.Cities.City_Type (City)));
         end if;
      end Add_City_Info;

   begin
      Manager.Planet.Scan_Cities (Add_City_Info'Access);
   end Load_Initial_State;

end Carthage.Managers.Cities;
