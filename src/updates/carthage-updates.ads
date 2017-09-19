package Carthage.Updates is

   type Update_Speed is range 0 .. 3;

   procedure Before_First_Update;

   procedure Start_Updates;
   procedure Stop_Updates;
   procedure Set_Speed (Speed : Update_Speed);

   procedure Render_Started;
   procedure Render_Finished;

   procedure Update;

end Carthage.Updates;
