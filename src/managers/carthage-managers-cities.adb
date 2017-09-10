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

         Info.Available.Clear_Stock;

         declare

            procedure Save_Quantity
              (Resource : Carthage.Resources.Resource_Type;
               Quantity : Natural);

            -------------------
            -- Save_Quantity --
            -------------------

            procedure Save_Quantity
              (Resource : Carthage.Resources.Resource_Type;
               Quantity : Natural)
            is
            begin
               Info.Available.Set_Quantity (Resource, Quantity);
            end Save_Quantity;

         begin
            Info.City.Scan_Stock (Save_Quantity'Access);
         end;
      end loop;

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

                     Info.Available.Remove
                       (Item.Resource, Natural'Min (Wanted, Available));

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
                                 if Quantity > 0 then
                                    City.Log
                                      ("transferring" & Quantity'Img
                                       & " " & Item.Resource.Identifier
                                       & " to " & Request.City.Identifier);
                                    City.Update.Transfer_Resource
                                      (Item.Resource, Quantity, Request.City);
                                 end if;
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

            procedure Store
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

            -----------
            -- Store --
            -----------

            procedure Store
              (Resource : Carthage.Resources.Resource_Type;
               Quantity : Natural)
            is
            begin
               Info.City.Log
                 ("store" & Quantity'Img & " " & Resource.Identifier);
               Info.City.Update.Transfer_Resource
                 (Resource, Quantity, Manager.Palace);
            end Store;

         begin
            if Info.City.Structure.Is_Harvester then
               if Manager.Palace /= null then
                  Info.Available.Scan_Stock (Store'Access);
               else
                  Info.Available.Scan_Stock (Sell'Access);
               end if;
            else
               declare
                  Inputs  : constant Carthage.Structures.Production_Array :=
                              Info.City.Structure.Production_Inputs;
                  Outputs : constant Carthage.Structures.Production_Array :=
                              Info.City.Structure.Production_Outputs;
               begin
                  for Item of Inputs loop
                     if Info.Available.Quantity (Item.Resource)
                       < 2 * Item.Quantity
                     then
                        Buy (Item.Resource,
                             2 * Item.Quantity
                             - Info.Available.Quantity (Item.Resource));
                     end if;
                  end loop;

                  for Item of Outputs loop
                     if Info.Available.Quantity (Item.Resource) > 0 then
                        if Manager.Palace /= null then
                           Store (Item.Resource,
                                  Info.Available.Quantity (Item.Resource));
                        else
                           Sell (Item.Resource,
                                 Info.Available.Quantity (Item.Resource));
                        end if;
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
     (Manager : not null access City_Manager_Record)
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
                 (City        =>
                      Carthage.Cities.City_Type (City),
                  Available   => <>,
                  Sources     => City_Resource_Lists.Empty_List,
                  Sinks       => City_Resource_Lists.Empty_List));
            if City.Structure.Is_Palace then
               Manager.House.Log (City.Identifier);
               Manager.Palace := Carthage.Cities.City_Type (City);
            elsif City.Structure.Is_Shield then
               Manager.Shield := Carthage.Cities.City_Type (City);
            end if;
         end if;
      end Add_City_Info;

   begin
      Manager.Planet.Scan_Cities (Add_City_Info'Access);
      Manager.Create_Resource_Network;
   end Load_Initial_State;

   -------------------------------
   -- Set_Resource_Requirements --
   -------------------------------

   procedure Set_Resource_Requirements
     (Manager : in out City_Manager_Record;
      Minimum : Carthage.Resources.Stock_Interface'Class;
      Desired : Carthage.Resources.Stock_Interface'Class;
      Result  : out Carthage.Resources.Stock_Interface'Class)
   is
      use type Carthage.Houses.House_Type;
      use type Carthage.Cities.City_Type;

      procedure Update_Result
        (Resource : Carthage.Resources.Resource_Type;
         Quantity : Natural);

      -------------------
      -- Update_Result --
      -------------------

      procedure Update_Result
        (Resource : Carthage.Resources.Resource_Type;
         Quantity : Natural)
      is
         Have           : constant Natural :=
                            Manager.Palace.Quantity (Resource);
         Desire         : constant Natural := Desired.Quantity (Resource);
         Final_Quantity : Natural;
      begin
         if Have = 0 then
            Final_Quantity := 0;
         elsif Quantity >= Have then
            Final_Quantity := Have;
         elsif Desire + Quantity < Have then
            Final_Quantity := Desire;
         else
            Final_Quantity := Quantity;
         end if;

         if Final_Quantity > 0 then
            Manager.Palace.Log
              (Resource.Name
               & ": have" & Have'Img
               & "; min" & Quantity'Img
               & ";desire" & Desire'Img
               & "; final quantity" & Final_Quantity'Img);

            Result.Add (Resource, Final_Quantity);
            Manager.Palace.Update.Remove (Resource, Final_Quantity);

            if Desire * 2 < Have then
               Manager.Palace.Update.Sell_Resource
                 (Resource, Have - Desire * 2);
            elsif Quantity * 2 > Have then
               Manager.Palace.Update.Buy_Resource
                 (Resource, Quantity * 2);
            end if;

         end if;

      end Update_Result;

   begin
      if Manager.Palace /= null
        and then Manager.Palace.Owner = Manager.House
      then
         Minimum.Scan_Stock (Update_Result'Access);
      end if;
   end Set_Resource_Requirements;

end Carthage.Managers.Cities;
