with Carthage.Houses;
with Carthage.Managers.Houses;

package body Carthage.Managers.Manager is

   procedure Start_House_Manager
     (House : Carthage.Houses.House_Type);

   -------------------------
   -- Start_House_Manager --
   -------------------------

   procedure Start_House_Manager
     (House : Carthage.Houses.House_Type)
   is
   begin
      Carthage.Managers.Houses.Create_House_Manager (House);
   end Start_House_Manager;

   --------------------
   -- Start_Managers --
   --------------------

   procedure Start_Managers is
   begin
      Carthage.Houses.Scan (Start_House_Manager'Access);
   end Start_Managers;

end Carthage.Managers.Manager;
