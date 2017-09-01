private with Ada.Containers.Doubly_Linked_Lists;

package Carthage.Managers.Houses is

   type House_Manager_Record is
     new Manager_Record with private;

   function Create_House_Manager
     (House  : Carthage.Houses.House_Type)
      return Manager_Type;

private

   type Planet_Record is
      record
         Planet           : Carthage.Planets.Planet_Type;
         Manager          : Manager_Type;
      end record;

   package List_Of_Planet_Records is
     new Ada.Containers.Doubly_Linked_Lists (Planet_Record);

   type House_Manager_Record is
     new Manager_Record with
      record
         Owned_Planets     : List_Of_Planet_Records.List;
      end record;

   overriding procedure Create_Initial_State
     (Manager : in out House_Manager_Record);

   overriding procedure Load_State
     (Manager : in out House_Manager_Record);

   overriding procedure Execute
     (Manager : in out House_Manager_Record);

end Carthage.Managers.Houses;
