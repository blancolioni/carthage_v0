with Ada.Numerics.Float_Random;

with Carthage.Calendar;

package body Carthage.Managers is

   Gen : Ada.Numerics.Float_Random.Generator;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Upd : Manager_Update)
   is
      use type Carthage.Calendar.Time;
      Next_Event_Delay : constant Duration :=
                           Upd.Manager.Update;
   begin
      if Next_Event_Delay > 0.0 then
         Upd.Queue (Carthage.Calendar.Clock + Next_Event_Delay);
      end if;
   end Activate;

   --------------
   -- Add_Goal --
   --------------

   procedure Add_Goal
     (Manager : in out Root_Manager_Type'Class;
      Goal    : Carthage.Goals.Goal_Record'Class)
   is
   begin
      Manager.Goals.Insert (Goal.Priority, Goal);
   end Add_Goal;

   -----------------
   -- Add_Manager --
   -----------------

   procedure Add_Manager
     (Manager : not null access Root_Manager_Type'Class)
   is
      use type Carthage.Calendar.Time;
      Upd : constant Manager_Update := (Manager => Manager_Type (Manager));
      First_Update_Delay : constant Duration :=
                             Duration
                               (Float (Manager.Average_Update_Frequency)
                                * (Ada.Numerics.Float_Random.Random (Gen)
                                    + 0.5));
   begin
      Upd.Queue (Carthage.Calendar.Clock + First_Update_Delay);
   end Add_Manager;

   -----------------
   -- Check_Goals --
   -----------------

   procedure Check_Goals
     (Manager : not null access Root_Manager_Type'Class)
   is
   begin
      while not Manager.Goals.Is_Empty loop
         if Manager.Check_Goal (Manager.Goals.First_Element) then
            Manager.Goals.Delete_First;
         else
            exit;
         end if;
      end loop;
   end Check_Goals;

   -------------------------------
   -- Get_Resource_Requirements --
   -------------------------------

   procedure Get_Resource_Requirements
     (Manager : in out Root_Manager_Type;
      Minimum : in out Carthage.Resources.Stock_Interface'Class;
      Desired : in out Carthage.Resources.Stock_Interface'Class)
   is
      pragma Unreferenced (Manager);
   begin
      Minimum.Clear_Stock;
      Desired.Clear_Stock;
   end Get_Resource_Requirements;

   function Have_Immediate_Capacity
     (Manager : Root_Manager_Type;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Manager, Goal);
   begin
      return False;
   end Have_Immediate_Capacity;

   ------------------------
   -- Transfer_Resources --
   ------------------------

   procedure Transfer_Resources
     (Manager : in out Root_Manager_Type;
      From    : in out Carthage.Resources.Stock_Interface'Class;
      Max     : Carthage.Resources.Stock_Interface'Class)
   is
      procedure Transfer
        (Resource : Carthage.Resources.Resource_Type);

      --------------
      -- Transfer --
      --------------

      procedure Transfer
        (Resource : Carthage.Resources.Resource_Type)
      is
         Transferred : constant Natural :=
                         Natural'Min
                           (From.Whole_Quantity (Resource),
                            Max.Whole_Quantity (Resource));
      begin
         if Transferred > 0 then
            Manager.Resources.Add (Resource, Transferred);
            From.Remove (Resource, Transferred);
         end if;
      end Transfer;

   begin
      Carthage.Resources.Scan (Transfer'Access);
   end Transfer_Resources;

end Carthage.Managers;
