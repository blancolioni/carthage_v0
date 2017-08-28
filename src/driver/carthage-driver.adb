with Carthage.Configure;
with Carthage.Logging;

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
   if True then
      Carthage.UI.Gtk_UI.Start;
   end if;
   Carthage.Logging.Stop_Logging;
end Carthage.Driver;
