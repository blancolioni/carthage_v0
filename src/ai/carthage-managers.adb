package body Carthage.Managers is

   --------------------------
   -- Before_Start_Of_Turn --
   --------------------------

   procedure Before_Start_Of_Turn is
   begin
      for Manager of Manager_List loop
         if Manager.First_Turn then
            Manager.Create_Initial_State;
            Manager.First_Turn := False;
         end if;
         Manager.Load_State;
      end loop;
   end Before_Start_Of_Turn;

   ------------
   -- Create --
   ------------

   procedure Create
     (Manager : in out Manager_Record'Class;
      House   : Carthage.Houses.House_Type)
   is
   begin
      Manager.House := House;
   end Create;

end Carthage.Managers;
