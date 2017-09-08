private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;

limited with Carthage.Managers.Planets;

with Carthage.Houses;
with Carthage.Planets;
with Carthage.Stacks;
with Carthage.Tiles;

package Carthage.Managers.Houses is

   type House_Manager_Record is
     abstract new Manager_Record
     and Carthage.Houses.House_Manager_Interface
   with private;

   procedure Add_Surface_Exploration_Goal
     (Manager : in out House_Manager_Record;
      Planet  : Carthage.Planets.Planet_Type);

   procedure Add_Planet_Scan_Goal
     (Manager : in out House_Manager_Record;
      Planet  : Carthage.Planets.Planet_Type);

   procedure Add_Planet_Capture_Goal
     (Manager : in out House_Manager_Record;
      Planet  : Carthage.Planets.Planet_Type);

   subtype House_Manager_Class is House_Manager_Record'Class;

   type House_Manager_Type is access all House_Manager_Record'Class;

   procedure Create_House_Manager
     (House  : Carthage.Houses.House_Type);

private

   type Managed_Planet_Record is
      record
         Planet        : Carthage.Planets.Planet_Type;
         Planet_Manager : access
           Carthage.Managers.Planets.Planet_Manager_Record'Class;
      end record;

   package Managed_Planet_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Managed_Planet_Record,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Goal_Class is (None, Explore_Surface, Planet_Scan, Capture_Planet);

   function Default_Priority
     (Class : Goal_Class)
      return Carthage.Goals.Goal_Priority
   is (case Class is
          when None => Carthage.Goals.Lowest_Priority,
          when Explore_Surface => 10,
          when Planet_Scan => 20,
          when Capture_Planet => 5);

   type House_Manager_Goal is
     new Carthage.Goals.Goal_Record with
      record
         Class  : Goal_Class := None;
         Planet : Carthage.Planets.Planet_Type;
      end record;

   type House_Manager_Record is
     abstract new Manager_Record
     and Carthage.Houses.House_Manager_Interface with
      record
         Planets : Managed_Planet_Maps.Map;
      end record;

   overriding procedure Load_Initial_State
     (Manager : in out House_Manager_Record);

   overriding procedure Check_Goals
     (Manager : in out House_Manager_Record);

   overriding procedure Execute_Turn
     (Manager : in out House_Manager_Record);

   overriding function Check_Goal
     (Manager : House_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

end Carthage.Managers.Houses;
