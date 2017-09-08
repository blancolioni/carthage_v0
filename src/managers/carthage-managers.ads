private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Carthage.Goals;
with Carthage.Houses;

package Carthage.Managers is

   type Manager_Record is abstract tagged private;

   procedure Load_Initial_State
     (Manager : in out Manager_Record)
   is abstract;

   function Check_Goal
     (Manager : Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
      is abstract;

   procedure Check_Goals
     (Manager : in out Manager_Record);

   procedure Execute_Turn (Manager : in out Manager_Record) is null;

   procedure Add_Goal
     (Manager : in out Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class);

   function Have_Immediate_Capacity
     (Manager : Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is (True);

   subtype Manager_Class is Manager_Record'Class;

   type Manager_Type is access all Manager_Record'Class;

   procedure Start_Managers;

   procedure Start_Manager_Turns;

   procedure Execute_Manager_Turns;

private

   package Goal_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Carthage.Goals.Goal_Record'Class, Carthage.Goals."=");

   function Higher_Priority
     (Left, Right : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is (Carthage.Goals."<" (Left.Priority, Right.Priority));

   package Sorting is
     new Goal_Lists.Generic_Sorting (Higher_Priority);

   type Manager_Record is abstract tagged
      record
         House : Carthage.Houses.House_Type;
         Goals : Goal_Lists.List;
      end record;

   package Manager_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Manager_Type);

   Top_Managers : Manager_Lists.List;

end Carthage.Managers;
