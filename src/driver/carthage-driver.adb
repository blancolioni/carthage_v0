with Carthage.Configure;
with Carthage.Logging;

with Carthage.UI.Gtk_UI;

procedure Carthage.Driver is
begin
   Carthage.Logging.Start_Logging ("carthage.log");
   Carthage.Configure.Load_Configuration;
   Carthage.Configure.Load_Scenario ("standard");
   if True then
      Carthage.UI.Gtk_UI.Start;
   end if;
   Carthage.Logging.Stop_Logging;
end Carthage.Driver;
