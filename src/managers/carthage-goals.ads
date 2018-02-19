package Carthage.Goals is

   type Goal_Priority is range 1 .. 100;

   Highest_Priority : constant Goal_Priority := Goal_Priority'First;
   Middle_Priority  : constant Goal_Priority := Goal_Priority'Last / 2;
   Lowest_Priority  : constant Goal_Priority := Goal_Priority'Last;

   type Goal_Record (Priority : Goal_Priority) is
     abstract tagged private;

   function Show (Goal : Goal_Record) return String is abstract;

private

   type Goal_Record (Priority : Goal_Priority) is
     abstract tagged null record;

end Carthage.Goals;
