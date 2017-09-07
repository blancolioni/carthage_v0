package body Carthage.Managers is

   --------------
   -- Add_Goal --
   --------------

   procedure Add_Goal
     (Manager : in out Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
   is
   begin
      Manager.Goals.Append (Goal);
   end Add_Goal;

   -----------------
   -- Check_Goals --
   -----------------

   procedure Check_Goals
     (Manager : in out Manager_Record)
   is
      use Goal_Lists;
      Position : Cursor;
   begin
      Sorting.Sort (Manager.Goals);
      Position := Manager.Goals.First;
      while Has_Element (Position) loop
         declare
            Current : Cursor := Position;
         begin
            Next (Position);
            if Manager_Record'Class (Manager).Check_Goal
              (Element (Current))
            then
               Manager.Goals.Delete (Current);
            end if;
         end;
      end loop;
   end Check_Goals;

   -------------------------
   -- Start_Manager_Turns --
   -------------------------

   procedure Start_Manager_Turns is
   begin
      for Manager of Top_Managers loop
         Manager.Check_Goals;
      end loop;
   end Start_Manager_Turns;

   --------------------
   -- Start_Managers --
   --------------------

   procedure Start_Managers is
   begin
      for Manager of Top_Managers loop
         Manager.Load_Initial_State;
      end loop;
   end Start_Managers;

end Carthage.Managers;
