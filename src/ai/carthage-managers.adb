package body Carthage.Managers is

   -----------------
   -- Add_Request --
   -----------------

   procedure Add_Request
     (Manager : in out Manager_Record;
      Request : Manager_Request)
   is
   begin
      Manager.Requests.Append (Request);
   end Add_Request;

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

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders is
   begin
      for Manager of Manager_List loop
         Manager.Execute;
      end loop;
   end Create_Orders;

end Carthage.Managers;
