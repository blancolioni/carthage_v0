private with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Houses;
with Carthage.Planets;

package Carthage.Managers is

   type Manager_Record is abstract tagged private;

   procedure Create
     (Manager : in out Manager_Record'Class;
      House   : Carthage.Houses.House_Type);

   procedure Create_Initial_State
     (Manager : in out Manager_Record)
   is abstract;

   procedure Load_State
     (Manager : in out Manager_Record)
   is abstract;

   subtype Manager_Class is Manager_Record'Class;

   type Manager_Type is access all Manager_Class;

   procedure Before_Start_Of_Turn;

private

   type Manager_Record is abstract tagged
      record
         House      : Carthage.Houses.House_Type;
         First_Turn : Boolean := True;
      end record;

   package Manager_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Manager_Type);

   Manager_List : Manager_Lists.List;

end Carthage.Managers;
