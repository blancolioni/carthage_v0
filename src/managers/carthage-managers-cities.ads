with Carthage.Cities;
with Carthage.Planets;
with Carthage.Resources;

package Carthage.Managers.Cities is

   type City_Manager_Record is
     new Manager_Record
     and Carthage.Cities.City_Manager_Interface
   with private;

   procedure Set_Resource_Requirements
     (Manager : in out City_Manager_Record;
      Minimum : Carthage.Resources.Stock_Interface'Class;
      Desired : Carthage.Resources.Stock_Interface'Class;
      Result  : out Carthage.Resources.Stock_Interface'Class);

   subtype City_Manager_Class is City_Manager_Record'Class;

   type City_Manager_Type is access all City_Manager_Record'Class;

   function Create_City_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return City_Manager_Type;

private

   type City_Resource is
      record
         City     : Carthage.Cities.City_Type;
         Resource : Carthage.Resources.Resource_Type;
      end record;

   type City_Request is
      record
         City     : Carthage.Cities.City_Type;
         Resource : Carthage.Resources.Resource_Type;
         Quantity : Resource_Quantity;
      end record;

   package City_Resource_Lists is
     new Ada.Containers.Doubly_Linked_Lists (City_Resource);

   package City_Request_Lists is
     new Ada.Containers.Doubly_Linked_Lists (City_Request);

   type City_Info_Record is
      record
         City      : Carthage.Cities.City_Type;
         Available : Carthage.Resources.Stock_Record;
         Ordered   : Carthage.Resources.Stock_Record;
         Sources   : City_Resource_Lists.List;
         Sinks     : City_Resource_Lists.List;
      end record;

   package City_Info_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (City_Info_Record);

   type City_Manager_Record is
     new Manager_Record
     and Carthage.Cities.City_Manager_Interface with
      record
         Planet : Carthage.Planets.Planet_Type;
         Palace : Carthage.Cities.City_Type;
         Shield : Carthage.Cities.City_Type;
         Cities : City_Info_Lists.List;
      end record;

   overriding procedure Load_Initial_State
     (Manager : not null access City_Manager_Record);

   overriding function Check_Goal
     (Manager : City_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is (True);

   overriding procedure On_Resource_Arrival
     (Manager  : in out City_Manager_Record;
      City     : not null access constant Carthage.Cities.City_Record'Class;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : Positive);

   procedure Create_Resource_Network
     (Manager : in out City_Manager_Record'Class);

   overriding procedure Execute_Turn
     (Manager : in out City_Manager_Record);

end Carthage.Managers.Cities;
