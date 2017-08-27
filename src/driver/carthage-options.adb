with WL.Command_Line;

package body Carthage.Options is

   pragma Style_Checks (Off);

   function Clear_Import_Cache return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("clear-import-cache", ' ');
   end Clear_Import_Cache;

end Carthage.Options;
