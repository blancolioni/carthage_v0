with Ada.Text_IO;

with Carthage.Configure;
with Carthage.Logging;
with Carthage.Updates;

with Carthage.Houses;
with Carthage.UI.Gtk_UI;

procedure Carthage.Driver is
   Fading_Suns_Scenario : constant Boolean := True;
begin
   Carthage.Logging.Start_Logging ("carthage.log");
   Carthage.Configure.Load_Configuration;
   if Fading_Suns_Scenario then
      Carthage.Configure.Load_Fading_Suns_Scenario;
   else
      Carthage.Configure.Load_Scenario ("standard");
   end if;

   Ada.Text_IO.Put_Line ("setting up first turn");
   Carthage.Updates.Before_Turn;
   Ada.Text_IO.Put_Line ("done");

   if True then
      Carthage.UI.Gtk_UI.Start
        (Carthage.Houses.Get ("li-halan"));
   end if;

   Carthage.Logging.Stop_Logging;
end Carthage.Driver;
