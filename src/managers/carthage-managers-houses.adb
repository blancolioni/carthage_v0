with WL.String_Sets;

with Carthage.Galaxy;

with Carthage.Managers.Assets;
with Carthage.Managers.Planets;

package body Carthage.Managers.Houses is

   type Passive_House_Manager_Record is
     new House_Manager_Record with null record;

   type Noble_House_Manager_Record is
     new House_Manager_Record with null record;

   overriding procedure Initialize
     (Manager : in out Noble_House_Manager_Record);

   overriding function Planet_Manager
     (Manager : Noble_House_Manager_Record;
      Planet  : Carthage.Planets.Planet_Type)
      return Manager_Type
   is (Carthage.Managers.Planets.Create_Active_Planet_Manager
       (Manager.House, Planet));

   --     overriding procedure Check_Goals
   --       (Manager : in out Noble_House_Manager_Record);

   type League_House_Manager_Record is
     new House_Manager_Record with null record;

   type Church_House_Manager_Record is
     new House_Manager_Record with null record;

--     ----------------------
--     -- Add_Capture_Goal --
--     ----------------------
--
--     procedure Add_Planet_Capture_Goal
--       (Manager : in out House_Manager_Record;
--        Planet  : Carthage.Planets.Planet_Type)
--     is
--     begin
--        Manager.House.Log ("goal: scan " & Planet.Name);
--  --        Manager.Goals.Append
--  --          (House_Manager_Goal'
--  --             (Carthage.Goals.Goal_Record with
--  --                  Priority => Default_Priority (Capture_Planet),
--  --                  Class    => Capture_Planet,
--  --                  Planet   => Planet));
--     end Add_Planet_Capture_Goal;
--
--     --------------------------
--     -- Add_Planet_Scan_Goal --
--     --------------------------
--
--     procedure Add_Planet_Scan_Goal
--       (Manager : in out House_Manager_Record;
--        Planet  : Carthage.Planets.Planet_Type)
--     is
--     begin
--        Manager.Goals.Append
--          (House_Manager_Goal'
--             (Carthage.Goals.Goal_Record with
--                  Priority => Default_Priority (Planet_Scan),
--                  Class    => Planet_Scan,
--                  Planet => Planet));
--     end Add_Planet_Scan_Goal;
--
--     ----------------------------------
--     -- Add_Surface_Exploration_Goal --
--     ----------------------------------
--
--     procedure Add_Surface_Exploration_Goal
--       (Manager : in out House_Manager_Record;
--        Planet  : Carthage.Planets.Planet_Type)
--     is
--     begin
--        Manager.House.Log ("goal: explore surface of " & Planet.Name);
--        Manager.Goals.Append
--          (House_Manager_Goal'
--             (Carthage.Goals.Goal_Record with
--                  Priority => Default_Priority (Explore_Surface),
--                  Class    => Explore_Surface,
--                  Planet   => Planet));
--     end Add_Surface_Exploration_Goal;
--
--     ----------------
--     -- Check_Goal --
--     ----------------
--
--     overriding function Check_Goal
--       (Manager : House_Manager_Record;
--        Goal    : Carthage.Goals.Goal_Record'Class)
--        return Boolean
--     is
--        House_Goal : House_Manager_Goal renames House_Manager_Goal (Goal);
--     begin
--        case House_Goal.Class is
--           when None =>
--              return True;
--           when Explore_Surface =>
--              declare
--                 Info : Managed_Planet_Record renames
--                          Manager.Planets (House_Goal.Planet.Identifier);
--              begin
--                 Info.Planet_Manager.Add_Surface_Exploration_Goal;
--              end;
--              return True;
--           when Planet_Scan =>
--              --  Manager.Space_Assets.Add_Planet_Scan_Goal (Goal.Planet);
--              return True;
--           when Capture_Planet =>
--              --  complicated!
--              return True;
--        end case;
--     end Check_Goal;
--
--     -----------------
--     -- Check_Goals --
--     -----------------
--
--     overriding procedure Check_Goals
--       (Manager : in out House_Manager_Record)
--     is
--     begin
--        Manager_Record (Manager).Check_Goals;
--
--        for Planet of Manager.Planets loop
--           Planet.Planet_Manager.Check_Goals;
--        end loop;
--     end Check_Goals;
--
--     -----------------
--     -- Check_Goals --
--     -----------------
--
--     overriding procedure Check_Goals
--       (Manager : in out Noble_House_Manager_Record)
--     is
--     begin
--        House_Manager_Record (Manager).Check_Goals;
--     end Check_Goals;

   --------------------------
   -- Create_House_Manager --
   --------------------------

   procedure Create_House_Manager
     (House  : Carthage.Houses.House_Type)
   is
      use Carthage.Houses;
      Manager : House_Manager_Type;
   begin
      case House.Category is
         when Noble =>
            Manager := new Noble_House_Manager_Record;
         when League =>
            Manager := new League_House_Manager_Record;
         when Church =>
            Manager := new Church_House_Manager_Record;
         when Rebels =>
            Manager := new Passive_House_Manager_Record;
         when Imperial =>
            Manager := new Passive_House_Manager_Record;
         when Vau =>
            Manager := new Passive_House_Manager_Record;
         when Symbiot =>
            Manager := new Passive_House_Manager_Record;
      end case;
      Manager.House := House;
      Manager.Space_Assets :=
        Carthage.Managers.Assets.Space_Asset_Manager
          (Manager, Manager.House);

      House.Update.Set_House_Manager (Manager);
      Manager.Initialize;
      Add_Manager (Manager);
   end Create_House_Manager;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : in out House_Manager_Record)
   is
      procedure Add_Planet_Info
        (Planet : Carthage.Planets.Planet_Type);

      procedure Add_Stack_Info
        (Stack : Carthage.Stacks.Stack_Type);

      ---------------------
      -- Add_Planet_Info --
      ---------------------

      procedure Add_Planet_Info
        (Planet : Carthage.Planets.Planet_Type)
      is
      begin
         Manager.Planets.Insert
           (Planet.Identifier,
            Managed_Planet_Record'
              (Planet         => Planet,
               Planet_Manager =>
                 House_Manager_Record'Class (Manager)
               .Planet_Manager (Planet)));
      end Add_Planet_Info;

      --------------------
      -- Add_Stack_Info --
      --------------------

      procedure Add_Stack_Info
        (Stack : Carthage.Stacks.Stack_Type)
      is
         use type Carthage.Houses.House_Type;
      begin
         if Stack.Owner = Manager.House then
            if not Manager.Planets.Contains
              (Stack.Planet.Identifier)
            then
               Manager.House.Log
                 ("create managers for planet " & Stack.Planet.Name);
               Add_Planet_Info (Stack.Planet);
            end if;
         end if;
      end Add_Stack_Info;

   begin

      Carthage.Stacks.Scan_Stacks (Add_Stack_Info'Access);

--        for Info of Manager.Planets loop
--           Info.Planet_Manager.Load_Initial_State;
--        end loop;

   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : in out Noble_House_Manager_Record)
   is

      Goal_Planets : WL.String_Sets.Set;

      procedure Add_Visit_Goal
        (Planet : Carthage.Planets.Planet_Type);

      --------------------
      -- Add_Visit_Goal --
      --------------------

      procedure Add_Visit_Goal
        (Planet : Carthage.Planets.Planet_Type)
      is
      begin
         if not Planet.Explored_By (Manager.House)
           and then not Planet.Seen_By (Manager.House)
           and then not Goal_Planets.Contains (Planet.Identifier)
         then
            Goal_Planets.Insert (Planet.Identifier);

            declare
               Goal : constant Carthage.Goals.Goal_Record'Class :=
                        Carthage.Managers.Assets.Planet_Reconnaissance_Goal
                          (Planet);
            begin
               if Manager.Space_Assets.Have_Immediate_Capacity (Goal) then
                  Manager.House.Log ("  adding goal: " & Goal.Show);
                  Manager.Space_Assets.Add_Goal (Goal);
               end if;
            end;

         end if;
      end Add_Visit_Goal;

   begin
      House_Manager_Record (Manager).Initialize;

      for Info of Manager.Planets loop
         declare
            use type Carthage.Houses.House_Type;
            Planet : constant Carthage.Planets.Planet_Type :=
                       Info.Planet;
         begin
            if Planet.Owner = Manager.House then
               Carthage.Galaxy.Scan_Connections
                 (Planet, Add_Visit_Goal'Access);
            end if;

            if Planet.Explored_By (Manager.House) then
               null;
--              elsif Planet.Seen_By (Manager.House) then
--                 Manager.Add_Surface_Exploration_Goal (Planet);
--              else
--                 Manager.Add_Planet_Scan_Goal (Planet);
            end if;
         end;
      end loop;
   end Initialize;

   --------------------
   -- Planet_Manager --
   --------------------

   function Planet_Manager
     (Manager : House_Manager_Record;
      Planet  : Carthage.Planets.Planet_Type)
      return Manager_Type
   is
   begin
      return Carthage.Managers.Planets.Create_Passive_Planet_Manager
        (Manager.House, Planet);
   end Planet_Manager;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Manager : not null access House_Manager_Record)
      return Duration
   is
   begin
      Manager.House.Log ("updating");
      return Carthage.Calendar.Days (1);
   end Update;

end Carthage.Managers.Houses;
