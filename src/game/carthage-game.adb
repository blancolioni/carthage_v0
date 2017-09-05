with Carthage.Calendar;

package body Carthage.Game is

   --------------
   -- New_Game --
   --------------

   procedure New_Game is
   begin
      Carthage.Calendar.Set_Date
        (Carthage.Calendar.Start_Year, 1, 1);
   end New_Game;

end Carthage.Game;
