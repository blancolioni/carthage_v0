with Carthage.Managers.Planets;

package body Carthage.Managers.Houses is

   type Noble_House_Manager_Record is new House_Manager_Record with
      record
         null;
      end record;

   type Church_House_Manager_Record is new House_Manager_Record with
      record
         null;
      end record;

   type League_House_Manager_Record is new House_Manager_Record with
      record
         null;
      end record;

   type Imperial_House_Manager_Record is new House_Manager_Record with
      record
         null;
      end record;

   type Symbiot_House_Manager_Record is new House_Manager_Record with
      record
         null;
      end record;

   type Vau_House_Manager_Record is new House_Manager_Record with
      record
         null;
      end record;

   type Rebel_House_Manager_Record is new House_Manager_Record with
      record
         null;
      end record;

   type Neutral_House_Manager_Record is new House_Manager_Record with
      record
         null;
      end record;

   --------------------------
   -- Create_House_Manager --
   --------------------------

   function Create_House_Manager
     (House  : Carthage.Houses.House_Type)
      return Manager_Type
   is
      use all type Carthage.Houses.House_Category;
      Manager : Manager_Type;

--        Noble, Church, League, Imperial,
--        Vau, Symbiot, Rebels, Neutral
   begin
      case House.Category is
         when Noble =>
            Manager := new Noble_House_Manager_Record;
         when Church =>
            Manager := new Church_House_Manager_Record;
         when League =>
            Manager := new League_House_Manager_Record;
         when Imperial =>
            Manager := new Imperial_House_Manager_Record;
         when Vau =>
            Manager := new Vau_House_Manager_Record;
         when Symbiot =>
            Manager := new Symbiot_House_Manager_Record;
         when Rebels =>
            Manager := new Rebel_House_Manager_Record;
         when Neutral =>
            Manager := new Neutral_House_Manager_Record;
      end case;

      Manager.Create (House);

      Manager_List.Append (Manager);

      return Manager;
   end Create_House_Manager;

   --------------------------
   -- Create_Initial_State --
   --------------------------

   overriding procedure Create_Initial_State
     (Manager : in out House_Manager_Record)
   is

      procedure Create_Planet_State
        (Planet : Carthage.Planets.Planet_Type);

      -------------------------
      -- Create_Planet_State --
      -------------------------

      procedure Create_Planet_State
        (Planet : Carthage.Planets.Planet_Type)
      is
         use type Carthage.Houses.House_Type;
      begin
         if Planet.Owner = Manager.House then
            declare
               Planet_Manager : constant Manager_Type :=
                                  Planets.Create_Planet_Manager
                                    (Manager.House, Planet);
            begin
               Planet_Manager.Create_Initial_State;
               Manager.Owned_Planets.Append
                 (Planet_Record'
                    (Planet  => Planet,
                     Manager => Planet_Manager));
            end;
         end if;
      end Create_Planet_State;

   begin
      Carthage.Planets.Scan (Create_Planet_State'Access);
   end Create_Initial_State;

   ----------------
   -- Load_State --
   ----------------

   overriding procedure Load_State
     (Manager : in out House_Manager_Record)
   is
   begin
      for Planet of Manager.Owned_Planets loop
         Planet.Manager.Load_State;
      end loop;
   end Load_State;

end Carthage.Managers.Houses;
