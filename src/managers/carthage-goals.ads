private with Ada.Containers.Indefinite_Holders;

package Carthage.Goals is

   type Goal_Priority is range 1 .. 100;

   Highest_Priority : constant Goal_Priority := Goal_Priority'First;
   Middle_Priority  : constant Goal_Priority := Goal_Priority'Last / 2;
   Lowest_Priority  : constant Goal_Priority := Goal_Priority'Last;

   type Goal_Record (Priority : Goal_Priority) is
     abstract tagged private;

   function Show (Goal : Goal_Record) return String is abstract;

   type Goal_Holder is tagged private;

   procedure Set_Goal (Holder : in out Goal_Holder'Class;
                       Goal   : Goal_Record'Class);

   function Goal (Holder : Goal_Holder'Class) return Goal_Record'Class;

private

   type Goal_Record (Priority : Goal_Priority) is
     abstract tagged null record;

   package Goal_Holders is
     new Ada.Containers.Indefinite_Holders (Goal_Record'Class);

   type Goal_Holder is new Goal_Holders.Holder with null record;

   function Goal (Holder : Goal_Holder'Class) return Goal_Record'Class
   is (Holder.Element);

end Carthage.Goals;
