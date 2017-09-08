with Ada.Containers.Doubly_Linked_Lists;

with WL.String_Maps;

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

   -----------------------------
   -- Create_Resource_Network --
   -----------------------------

   procedure Create_Resource_Network
     (Manager : in out City_Manager_Record'Class)
   is
      use Carthage.Cities, Carthage.Structures;
   begin
      for Info of Manager.Cities loop
         Info.City.Log ("creating resource network");
         declare
            City      : constant City_Type := Info.City;
            Structure : constant Structure_Type := City.Structure;
            Inputs    : constant Production_Array :=
                          Structure.Production_Inputs;
         begin
            for Source_Info of Manager.Cities loop
               for Item of Inputs loop
                  if Source_Info.City.Structure.Produces (Item.Resource) then
                     Info.Sources.Append ((Source_Info.City, Item.Resource));
                     Source_Info.Sinks.Append ((Info.City, Item.Resource));
                     City.Log ("receiving " & Item.Resource.Name
                               & " from " & Source_Info.City.Identifier);
                  end if;
               end loop;
            end loop;
         end;
      end loop;
   end Create_Resource_Network;

   ------------------
   -- Execute_Turn --
   ------------------

   overriding procedure Execute_Turn
     (Manager : in out City_Manager_Record)
   is
      use Carthage.Cities, Carthage.Structures;
      use type Carthage.Resources.Resource_Type;

      package Request_Maps is
        new WL.String_Maps (City_Request_Lists.List, City_Request_Lists."=");

      Requests : Request_Maps.Map;

   begin
      for Info of Manager.Cities loop
         declare
            City : constant City_Type := Info.City;
            Structure : constant Structure_Type := City.Structure;
         begin
            if not Structure.Is_Harvester then
               declare
                  Inputs : constant Production_Array :=
                             Structure.Production_Inputs;
               begin
                  for Item of Inputs loop
                     if City.Quantity (Item.Resource) < 2 * Item.Quantity then
                        for Source of Info.Sources loop
                           if Source.Resource = Item.Resource then
                              if not Requests.Contains
                                (Source.City.Identifier)
                              then
                                 Requests.Insert
                                   (Source.City.Identifier,
                                    City_Request_Lists.Empty_List);
                              end if;

                              Requests (Source.City.Identifier).Append
                                ((City, Item.Resource,
                                 2 * Item.Quantity
                                 - City.Quantity (Item.Resource)));
                           end if;
                        end loop;
                     end if;
                  end loop;
               end;
            end if;
         end;
      end loop;

      for Info of Manager.Cities loop
         declare
            use Carthage.Resources;
            City      : constant City_Type := Info.City;
            Structure : constant Structure_Type := City.Structure;
         begin
            if Requests.Contains (City.Identifier) then
               for Item of Structure.Production_Outputs loop
                  declare
                     Wanted    : Natural := 0;
                     Available : constant Natural :=
                                   City.Quantity (Item.Resource);

                  begin
                     for Request of Requests.Element (City.Identifier) loop
                        if Request.Resource = Item.Resource then
                           Wanted := Wanted + Request.Quantity;
                        end if;
                     end loop;

                     if Wanted <= Available then
                        Wanted := Available;
                     end if;

                     if Wanted > 0 and then Available > 0 then
                        for Request of Requests.Element (City.Identifier) loop
                           if Request.Resource = Item.Resource then
                              declare
                                 Quantity : constant Natural :=
                                              Request.Quantity
                                                * Available / Wanted;
                              begin
                                 City.Log ("transferring" & Quantity'Img
                                           & " " & Item.Resource.Identifier
                                           & " to " & Request.City.Identifier);
                                 City.Update.Remove (Item.Resource, Quantity);
                                 Request.City.Update.Add
                                   (Item.Resource, Quantity);
                              end;
                           end if;
                        end loop;
                     end if;
                  end;
               end loop;
            end if;
         end;

      end loop;

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
   end Execute_Turn;

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
                 (City     =>
                      Carthage.Cities.City_Type (City),
                  Sources  => City_Resource_Lists.Empty_List,
                  Sinks    => City_Resource_Lists.Empty_List));
            if City.Structure.Name = "palace" then
               Manager.Palace := Carthage.Cities.City_Type (City);
            elsif City.Structure.Name = "agora" then
               Manager.Agora := Carthage.Cities.City_Type (City);
            elsif City.Structure.Name = "shield" then
               Manager.Shield := Carthage.Cities.City_Type (City);
            end if;
         end if;
      end Add_City_Info;

   begin
      Manager.Planet.Scan_Cities (Add_City_Info'Access);
      Manager.Create_Resource_Network;
   end Load_Initial_State;

end Carthage.Managers.Cities;
