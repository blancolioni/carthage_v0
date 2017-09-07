with Ada.Text_IO;

with WL.Command_Line;

with Carthage.Configure;
with Carthage.Logging;
with Carthage.Updates;

with Carthage.Houses;
with Carthage.UI.Gtk_UI;

with Carthage.Managers.Houses;

with Carthage.Game;

with Carthage.Options;
with Carthage.Paths;

procedure Carthage.Driver is
   Fading_Suns_Scenario : constant Boolean := True;
begin
   Carthage.Logging.Start_Logging ("carthage.log");
   WL.Command_Line.Load_Defaults
     (Carthage.Paths.Config_File ("default-options.txt"));

   Carthage.Configure.Load_Configuration;
   if Fading_Suns_Scenario then
      Carthage.Configure.Load_Fading_Suns_Scenario;
   else
      Carthage.Configure.Load_Scenario ("standard");
   end if;

   Carthage.Game.New_Game;

   Ada.Text_IO.Put_Line ("setting up first turn");
   Carthage.Updates.Before_First_Turn;
   Ada.Text_IO.Put_Line ("done");

   Ada.Text_IO.Put_Line ("setting up house managers");

   declare
      procedure Create_Manager (House : Carthage.Houses.House_Type);

      --------------------
      -- Create_Manager --
      --------------------

      procedure Create_Manager (House : Carthage.Houses.House_Type) is
      begin
         Carthage.Managers.Houses.Create_House_Manager (House);
      end Create_Manager;

   begin
      Carthage.Houses.Scan (Create_Manager'Access);
   end;

   Carthage.Managers.Start_Managers;

   Ada.Text_IO.Put_Line ("done");

   if Carthage.Options.Wizard_Mode then
      Carthage.UI.Set_Wizard_Mode (True);
   end if;

   if Carthage.Options.Update then
      for I in 1 .. Natural'Max (Carthage.Options.Update_Count, 1) loop
         Carthage.Updates.Update;
      end loop;
   end if;

   if Carthage.Options.Gtk_UI then
      Carthage.UI.Gtk_UI.Start
        (Carthage.Houses.Get (Carthage.Options.House));
   end if;

   Carthage.Logging.Stop_Logging;
end Carthage.Driver;
