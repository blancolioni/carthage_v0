package body Carthage.Technology.Configure is

   --------------------------
   -- Configure_Technology --
   --------------------------

   procedure Configure_Technology
     (Config : Tropos.Configuration)
   is
      procedure Create (T : in out Technology_Class);

      ------------
      -- Create --
      ------------

      procedure Create (T : in out Technology_Class) is
      begin
         T.Create_With_Identity (Config.Config_Name);
         T.Index := Config.Get ("index");
         T.Like := Config.Get ("like");
         T.Cost := Config.Get ("cost");
      end Create;

      T : constant Technology_Type := Db.Create (Create'Access);
   begin
      while Tech.Last_Index < T.Index loop
         Tech.Append (null);
      end loop;
      Tech.Replace_Element (T.Index, T);
   end Configure_Technology;

   --------------------
   -- Configure_Tree --
   --------------------

   procedure Configure_Tree
     (Config : Tropos.Configuration)
   is
      procedure Update (T : in out Technology_Class);

      ------------
      -- Update --
      ------------

      procedure Update (T : in out Technology_Class) is
         Index : Natural := 0;
      begin
         for Enabled_Config of Config loop
            if Enabled_Config.Config_Name = "enabled-by" then
               declare
                  Enabler : constant Natural :=
                              Enabled_Config.Value;
               begin
                  T.Top_Level := Enabler >= 800;

                  if not T.Top_Level then
                     Index := Index + 1;
                     T.Enabled_By (Index) := Tech.Element (Enabler);
                  end if;
               end;
            end if;
         end loop;
      end Update;

   begin
      Db.Update (Get (Config.Config_Name).Reference, Update'Access);
   end Configure_Tree;

end Carthage.Technology.Configure;
