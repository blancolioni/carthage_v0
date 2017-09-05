with Ada.Containers.Indefinite_Holders;

with Carthage.Stacks;

package body Carthage.Managers.Assets is

   package Request_Holders is
     new Ada.Containers.Indefinite_Holders (Manager_Request);

   type Stack_Info_Record is
      record
         Stack      : Carthage.Stacks.Stack_Type;
         Has_Orders : Boolean;
         Request    : Request_Holders.Holder;
      end record;

   package Stack_Info_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Stack_Info_Record);

   type Asset_Manager_Record is
     abstract new Manager_Record with
      record
         Planet  : Carthage.Planets.Planet_Type;
         Stacks  : Stack_Info_Lists.List;
      end record;

   type Ground_Asset_Manager_Record is
     new Asset_Manager_Record with
      record
         null;
      end record;

   overriding procedure Create_Initial_State
     (Manager : in out Ground_Asset_Manager_Record);

   overriding procedure Load_State
     (Manager : in out Ground_Asset_Manager_Record)
   is null;

   overriding procedure Execute
     (Manager : in out Ground_Asset_Manager_Record);

   function Execute_Tile_Recon_Request
     (Manager : in out Ground_Asset_Manager_Record'Class;
      Tile    : Carthage.Tiles.Tile_Type)
      return Boolean;

   --------------------------
   -- Create_Initial_State --
   --------------------------

   overriding procedure Create_Initial_State
     (Manager : in out Ground_Asset_Manager_Record)
   is

      use type Carthage.Houses.House_Type;

      procedure Process (Stack : Carthage.Stacks.Stack_Type);

      -------------
      -- Process --
      -------------

      procedure Process (Stack : Carthage.Stacks.Stack_Type) is
         use Carthage.Planets;
      begin
         if Stack.Planet = Manager.Planet
           and then not Stack.In_Space
           and then Stack.Owner = Manager.House
         then
            Manager.Stacks.Append
              (Stack_Info_Record'
                 (Stack      => Stack,
                  Has_Orders => False,
                  Request    => <>));
         end if;
      end Process;

   begin
      Carthage.Stacks.Scan_Stacks (Process'Access);
   end Create_Initial_State;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Manager : in out Ground_Asset_Manager_Record)
   is
      New_Requests : Request_Lists.List;
   begin
      for Request of Manager.Requests loop
         declare
            Done : Boolean;
         begin
            case Request.Class is
               when Tile_Recon =>
                  Done := Execute_Tile_Recon_Request (Manager, Request.Tile);
            end case;

            if not Done then
               New_Requests.Append (Request);
            end if;
         end;
      end loop;
      Manager.Requests := New_Requests;
   end Execute;

   --------------------------------
   -- Execute_Tile_Recon_Request --
   --------------------------------

   function Execute_Tile_Recon_Request
     (Manager : in out Ground_Asset_Manager_Record'Class;
      Tile    : Carthage.Tiles.Tile_Type)
      return Boolean
   is
      use Stack_Info_Lists;
      Closest      : Cursor := No_Element;
      Min_Distance : Natural := Natural'Last;
   begin

      for Item in Manager.Stacks.Iterate loop
         if not Element (Item).Has_Orders then
            declare
               D : constant Natural :=
                     Carthage.Planets.Hex_Distance
                       (Element (Item).Stack.Tile.Position, Tile.Position);
            begin
               if D < Min_Distance then
                  Closest := Item;
                  Min_Distance := D;
               end if;
            end;
         end if;
      end loop;

      if Closest = No_Element then
         return False;
      else
         declare
            Info : Stack_Info_Record := Element (Closest);
         begin
            Info.Stack.Update.Move_To_Tile (Tile);
            Info.Has_Orders := True;
            Manager.Stacks.Replace_Element (Closest, Info);
         end;
         return True;
      end if;
   end Execute_Tile_Recon_Request;

   --------------------------
   -- Ground_Asset_Manager --
   --------------------------

   function Ground_Asset_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Manager_Type
   is
      Manager : Ground_Asset_Manager_Record;
   begin
      Manager.Create (House);
      Manager.Planet := Planet;
      return new Ground_Asset_Manager_Record'(Manager);
   end Ground_Asset_Manager;

end Carthage.Managers.Assets;
