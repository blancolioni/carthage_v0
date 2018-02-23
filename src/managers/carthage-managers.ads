private with WL.Indefinite_Heaps;
private with Carthage.Updates;

with Carthage.Goals;
with Carthage.Resources;

with Carthage.Houses;

package Carthage.Managers is

   type Root_Manager_Type is abstract tagged private;

   type Manager_Type is access all Root_Manager_Type'Class;

   function Description
     (Manager : Root_Manager_Type)
      return String
   is ("manager");

   procedure Initialize
     (Manager : in out Root_Manager_Type)
   is null;

   function Have_Immediate_Capacity
     (Manager : Root_Manager_Type;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is (False);

   procedure Get_Resource_Requirements
     (Manager : in out Root_Manager_Type;
      Minimum : in out Carthage.Resources.Stock_Interface'Class;
      Desired : in out Carthage.Resources.Stock_Interface'Class);

   procedure Transfer_Resources
     (Manager : in out Root_Manager_Type;
      From    : in out Carthage.Resources.Stock_Interface'Class;
      Max     : Carthage.Resources.Stock_Interface'Class);

   function Check_Goal
     (Manager : not null access Root_Manager_Type;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;
   --  Return True if goal has been processed and can be removed
   --  False if goal has not been processed and checking should stop

   procedure Add_Goal
     (Manager : in out Root_Manager_Type'Class;
      Goal    : Carthage.Goals.Goal_Record'Class);

   procedure Check_Goals
     (Manager : not null access Root_Manager_Type'Class);

   function Average_Update_Frequency
     (Manager : Root_Manager_Type)
      return Duration
      is abstract;

   function Update
     (Manager : not null access Root_Manager_Type)
      return Duration
      is abstract;

private

   package Goal_Queues is
     new WL.Indefinite_Heaps
       (Key_Type     => Carthage.Goals.Goal_Priority,
        Element_Type => Carthage.Goals.Goal_Record'Class,
        "<"          => Carthage.Goals."<",
        "="          => Carthage.Goals."=");

   type Root_Manager_Type is abstract tagged
      record
         House     : Carthage.Houses.House_Type;
         Goals     : Goal_Queues.Heap;
         Resources : Carthage.Resources.Stock_Record;
      end record;

   function Check_Goal
     (Manager : not null access Root_Manager_Type;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is (True);

   type Manager_Update is
     new Carthage.Updates.Update_Interface with
      record
         Manager : Manager_Type;
      end record;

   overriding procedure Activate
     (Upd : Manager_Update);

   procedure Add_Manager
     (Manager : not null access Root_Manager_Type'Class);

end Carthage.Managers;
