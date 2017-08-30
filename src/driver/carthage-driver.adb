with Ada.Text_IO;

with Carthage.Configure;
with Carthage.Logging;
with Carthage.Updates;

with Carthage.Houses;
with Carthage.UI.Gtk_UI;

with Carthage.Managers.Houses;

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
   Carthage.Updates.Before_First_Turn;
   Ada.Text_IO.Put_Line ("done");

   Ada.Text_IO.Put_Line ("setting up house managers");
   declare
      procedure Set_Manager (House : Carthage.Houses.House_Type);

      -----------------
      -- Set_Manager --
      -----------------

      procedure Set_Manager (House : Carthage.Houses.House_Type) is
      begin
         Carthage.Houses.Set_House_Manager
           (House, Carthage.Managers.Houses.Create_House_Manager (House));
      end Set_Manager;

   begin
      Carthage.Houses.Scan (Set_Manager'Access);
   end;
   Ada.Text_IO.Put_Line ("done");

   if True then
      Carthage.UI.Gtk_UI.Start
        (Carthage.Houses.Get ("li-halan"));
   end if;

   Carthage.Logging.Stop_Logging;
end Carthage.Driver;
