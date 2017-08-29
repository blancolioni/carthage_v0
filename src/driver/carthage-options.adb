with WL.Command_Line;

package body Carthage.Options is

   pragma Style_Checks (Off);

   function Clear_Import_Cache return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("clear-import-cache", ' ');
   end Clear_Import_Cache;

   function Trace_Planet_Import return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("trace-planet-import", ' ');
   end Trace_Planet_Import;

   function Trace_Unit_Import return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("trace-unit-import", ' ');
   end Trace_Unit_Import;

end Carthage.Options;
