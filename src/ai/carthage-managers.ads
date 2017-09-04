private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Carthage.Houses;
with Carthage.Planets;
with Carthage.Tiles;

package Carthage.Managers is

   type Manager_Request (<>) is private;

   function Tile_Recon_Request
     (Planet : Carthage.Planets.Planet_Type;
      Tile   : Carthage.Tiles.Tile_Type)
      return Manager_Request;

   type Manager_Record is abstract tagged private;

   procedure Create
     (Manager : in out Manager_Record'Class;
      House   : Carthage.Houses.House_Type);

   procedure Add_Request
     (Manager : in out Manager_Record;
      Request : Manager_Request);

   procedure Create_Initial_State
     (Manager : in out Manager_Record)
   is abstract;

   procedure Load_State
     (Manager : in out Manager_Record)
   is abstract;

   procedure Execute (Manager : in out Manager_Record) is abstract;

   subtype Manager_Class is Manager_Record'Class;

   type Manager_Type is access all Manager_Class;

   procedure Before_Start_Of_Turn;
   procedure Create_Orders;

private

   type Request_Class is (Tile_Recon);

   type Manager_Request (Class : Request_Class) is
      record
         Planet : Carthage.Planets.Planet_Type;
         Tile   : Carthage.Tiles.Tile_Type;
      end record;

   function Tile_Recon_Request
     (Planet : Carthage.Planets.Planet_Type;
      Tile   : Carthage.Tiles.Tile_Type)
      return Manager_Request
   is (Tile_Recon, Planet, Tile);

   package Request_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Manager_Request);

   type Manager_Record is abstract tagged
      record
         House      : Carthage.Houses.House_Type;
         First_Turn : Boolean := True;
         Requests   : Request_Lists.List;
      end record;

   package Manager_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Manager_Type);

   Manager_List : Manager_Lists.List;

end Carthage.Managers;
