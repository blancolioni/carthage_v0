with WL.Command_Line;

package body Carthage.Options is

   pragma Style_Checks (Off);

   function Clear_Import_Cache return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("clear-import-cache", ' ');
   end Clear_Import_Cache;

   function Gtk_UI return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("gtk-ui", ' ');
   end Gtk_UI;

   function House return String is
   begin
      return WL.Command_Line.Find_Option
               ("house", ' ');
   end House;

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

   function Update return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("update", ' ');
   end Update;

   function Update_Count return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("update-count", ' ', 1);
   end Update_Count;

   function Wizard_Mode return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("wizard-mode", ' ');
   end Wizard_Mode;

   function Show_Hex_Coordinates return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("show-hex-coordinates", ' ');
   end Show_Hex_Coordinates;

   function Show_Cubic_Coordinates return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("show-cubic-coordinates", ' ');
   end Show_Cubic_Coordinates;

   function Combat_Test return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("combat-test", ' ');
   end Combat_Test;

end Carthage.Options;
