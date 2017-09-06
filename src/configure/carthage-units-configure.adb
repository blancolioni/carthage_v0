with Ada.Exceptions;

package body Carthage.Units.Configure is

   function To_Weapon_Category
     (Name : String)
      return Weapon_Category;

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (Config : Tropos.Configuration)
   is
      procedure Create (Unit : in out Unit_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Unit : in out Unit_Class) is
      begin
         Unit.Create_With_Identity (Config.Config_Name);
         Unit.Index := Config.Get ("index");
         Unit.Category := Unit_Category'Value (Config.Get ("category"));
         Unit.Move := Config.Get ("move", 0);
         Unit.Spot := Config.Get ("spot", 0);
         Unit.Camouflage := Config.Get ("camouflage", 0);
         Unit.Agility := Config.Get ("agility", 0);
         Unit.Armour := Config.Get ("armour", 0);
         Unit.Psy_Defence := Config.Get ("psy-defence", 0);
         Unit.Cargo := Config.Get ("cargo", 0);
         Unit.Maintenance := Config.Get ("maintenance", 0);
         Unit.Credit_Cost := Config.Get ("credit-cost", 0);
         Unit.Turns_To_Build := Config.Get ("turns-to-build", 0);
         Unit.Can_Be_Cargo := Config.Get ("can-be-cargo", False);
         Unit.Combat := not Config.Get ("non-combat", False);
         Unit.Eat := Config.Get ("eat");
         Unit.Rank := Config.Get ("rank");

         Unit.Weapons := (others => (0, 0));

         if Config.Contains ("attack") then
            declare
               Attack : constant Tropos.Configuration :=
                          Config.Child ("attack");
            begin
               for Cat of Attack loop
                  declare
                     Category : constant Weapon_Category :=
                                  To_Weapon_Category (Cat.Config_Name);
                     Accuracy : constant Natural :=
                                  Cat.Get ("accuracy");
                     Strength : constant Natural :=
                                  Cat.Get ("strength");
                  begin
                     Unit.Weapons (Category) := (Accuracy, Strength);
                  end;
               end loop;
            end;
         end if;

         Unit.Image_Resource :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (String'(Config.Get ("icon")));
      end Create;

      Unit : constant Unit_Type :=
               Db.Create (Create'Access);

   begin
      while Us.Last_Index < Unit.Index loop
         Us.Append (null);
      end loop;

      Us.Replace_Element (Unit.Index, Unit);

   exception
      when E : others =>
         raise Constraint_Error with "error while configuring "
           & Config.Config_Name & ": "
           & Ada.Exceptions.Exception_Message (E);
   end Configure_Unit;

   ------------------------
   -- To_Weapon_Category --
   ------------------------

   function To_Weapon_Category
     (Name : String)
      return Weapon_Category
   is
   begin
      if Name = "close-space" then
         return Close_Space;
      elsif Name = "direct-space" then
         return Direct_Space;
      elsif Name = "ranged-space" then
         return Ranged_Space;
      else
         return Weapon_Category'Value (Name);
      end if;
   exception
      when others =>
         raise Constraint_Error with
           "unknown weapon category: " & Name;
   end To_Weapon_Category;

end Carthage.Units.Configure;
