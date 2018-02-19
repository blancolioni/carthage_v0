package body Carthage.Resources.Configure is

   ------------------------
   -- Configure_Resource --
   ------------------------

   procedure Configure_Resource
     (Config : Tropos.Configuration)
   is
      procedure Create (Resource : in out Resource_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Resource : in out Resource_Class) is
      begin
         Resource.Create_With_Identity (Config.Config_Name);
         Resource.Index := Resource_Vector.Last_Index + 1;
         Resource.Price := Config.Get ("price");
      end Create;

      Resource : constant Resource_Type :=
                   Db.Create (Create'Access);
   begin
      Resource_Vector.Append (Resource);
   end Configure_Resource;

end Carthage.Resources.Configure;
