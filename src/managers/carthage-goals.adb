package body Carthage.Goals is

   --------------
   -- Set_Goal --
   --------------

   procedure Set_Goal
     (Holder : in out Goal_Holder'Class;
      Goal   : Goal_Record'Class)
   is
   begin
      Holder.Replace_Element (Goal);
   end Set_Goal;

end Carthage.Goals;
