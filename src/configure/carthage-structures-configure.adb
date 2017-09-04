with WL.Random;

with Tropos.Reader;

with Carthage.Configure;

package body Carthage.Structures.Configure is

   procedure Configure_Production
     (Production : in out Production_Lists.List;
      Config     : Tropos.Configuration);

   procedure Configure_Production
     (Structure_Name : String;
      Inputs        : in out Production_Lists.List;
      Outputs       : in out Production_Lists.List;
      Config        : Tropos.Configuration);

   --------------------------------
   -- Configure_Bonus_Production --
   --------------------------------

   procedure Configure_Bonus_Production
     (Config : Tropos.Configuration)
   is
   begin

      for Structure_Config of Config loop
         declare
            Item : constant Structure_Type :=
                     Get (Structure_Config.Config_Name);
            List : Bonus_Production_Lists.List;
         begin

            for Bonus_Config of Structure_Config loop
               declare
                  Bonus_Structure : constant Structure_Type :=
                                      Get (Bonus_Config.Config_Name);
               begin
                  for Resource_Config of Bonus_Config loop
                     declare
                        use Carthage.Resources;
                        Resource : constant Resource_Type :=
                                     Get (Resource_Config.Config_Name);
                        Quantity : constant Natural :=
                                     Resource_Config.Value;
                     begin
                        List.Append
                          (Bonus_Production_Record'
                             (Bonus_Structure => Bonus_Structure,
                              Resource        => Resource,
                              Quantity        => Quantity));
                     end;
                  end loop;
               end;
            end loop;

            declare
               procedure Set_List (Rec : in out Structure_Class);

               --------------
               -- Set_List --
               --------------

               procedure Set_List (Rec : in out Structure_Class) is
               begin
                  Rec.Bonus := List;
               end Set_List;

            begin
               Db.Update (Item.Reference, Set_List'Access);
            end;
         end;

      end loop;
   end Configure_Bonus_Production;

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Production : in out Production_Lists.List;
      Config     : Tropos.Configuration)
   is
      type State_Type is
        (Terrain_State, World_State, Resource_State, Count_State);
   begin
      for Item_Config of Config loop
         declare
            State    : State_Type := Terrain_State;
            Terrain  : Carthage.Terrain.Terrain_Type;
            World    : Carthage.Worlds.World_Type;
            City     : Boolean := False;
            Resource : Carthage.Resources.Resource_Type;
            Count    : Natural;
         begin
            for Field_Config of Item_Config loop
               declare
                  Field : String := Field_Config.Config_Name;
               begin
                  for Ch of Field loop
                     if Ch = ' ' then
                        Ch := '_';
                     end if;
                  end loop;

                  case State is
                     when Terrain_State =>
                        if not Carthage.Terrain.Exists (Field) then
                           raise Constraint_Error with
                             "production: no such terrain type: "
                             & Field;
                        end if;
                        Terrain := Carthage.Terrain.Get (Field);
                        State := World_State;
                     when World_State =>
                        if Field = "city" then
                           City := True;
                        elsif not Carthage.Worlds.Exists (Field) then
                           raise Constraint_Error with
                             "production: no such world type: "
                             & Field;
                        else
                           World := Carthage.Worlds.Get (Field);
                        end if;
                        State := Resource_State;
                     when Resource_State =>
                        if Field = "@" then
                           State := World_State;
                        else
                           Resource := Carthage.Resources.Get (Field);
                           State := Count_State;
                        end if;
                     when Count_State =>
                        Count := Natural'Value (Field);
                        State := Resource_State;
                        Production.Append
                          (Production_Record'
                             (World    => World,
                              City     => City,
                              Terrain  => Terrain,
                              Resource => Resource,
                              Quantity => Count));
                        City := False;
                        World := null;
                  end case;
               end;
            end loop;
         end;
      end loop;
   end Configure_Production;

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Structure_Name : String;
      Inputs        : in out Production_Lists.List;
      Outputs       : in out Production_Lists.List;
      Config        : Tropos.Configuration)
   is
   begin
      Inputs.Clear;
      Outputs.Clear;
      for Item of Config.Child ("input") loop
         Inputs.Append
           (Production_Record'
              (Resource => Carthage.Resources.Get (Item.Config_Name),
               Quantity => Item.Value,
               others   => <>));
      end loop;
      for Item of Config.Child ("output") loop
         Outputs.Append
           (Production_Record'
              (Resource => Carthage.Resources.Get (Item.Config_Name),
               Quantity => Item.Value,
               others   => <>));
      end loop;
      if Inputs.Is_Empty and then Outputs.Is_Empty then
         raise Constraint_Error with
           "while configuring production for "
           & Structure_Name & ": cannot find production";
      end if;

   end Configure_Production;

   ------------------------
   -- Configure_Structure --
   ------------------------

   procedure Configure_Structure
     (Config : Tropos.Configuration)
   is
      procedure Create (Structure : in out Structure_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Structure : in out Structure_Class) is
      begin
         Structure.Create_With_Identity (Config.Config_Name);
         Structure.Index := Config.Get ("index");
         Structure.Shield := Config.Get ("shield");
         Structure.Palace := Config.Get ("palace");
         Structure.Agora := Config.Get ("agora");
         Structure.Church := Config.Get ("church");
         Structure.Can_Build := Config.Get ("can_build");
         Structure.Water := Config.Get ("water");
         Structure.Land := Config.Get ("land");
         Structure.Road := Config.Get ("road");
         Structure.Barren := Config.Get ("barren");
         Structure.Neutral := Config.Get ("neutral");
         Structure.Area := Config.Get ("area", 0);
         Structure.Maintenance := Config.Get ("maintenance", 0);
         Structure.Cost := Config.Get ("cost", 0);
         Structure.Build_Time := Config.Get ("build-time");
         Structure.Enabled_By :=
           Carthage.Technology.Get (Config.Get ("enabled-by", "nothing"));

         Structure.Is_Harvester := Config.Get ("harvester");

         if Config.Contains ("production") then
            if Structure.Is_Harvester then
               declare
                  Harvest_Name : constant String :=
                                   Config.Get ("production");
               begin
                  Configure_Production
                    (Structure.Production,
                     Tropos.Reader.Read_Config
                       (Carthage.Configure.Fading_Suns_Data_File
                            (Harvest_Name)));
               end;
            else
               Configure_Production
                 (Structure.Identifier,
                  Structure.Inputs, Structure.Production,
                  Config.Child ("production"));
            end if;
         end if;

         if Config.Contains ("bonus") then
            Structure.Is_Bonus := True;
            for Bonus_Config of Config.Child ("bonus") loop
               Structure.Chances.Insert
                 (Bonus_Config.Config_Name,
                  Bonus_Config.Value);
            end loop;
         end if;
      end Create;

      Structure : constant Structure_Type :=
                    Db.Create (Create'Access);
   begin
      while Fs.Last_Index < Structure.Index loop
         Fs.Append (null);
      end loop;
      Fs.Replace_Element (Structure.Index, Structure);
   end Configure_Structure;

   ------------------
   -- Random_Bonus --
   ------------------

   function Random_Bonus
     (Planet_Category : String;
      Terrain_Id      : String)
      return Structure_Type
   is
   begin
      for Structure of Fs loop
         declare
            Chance : constant Natural :=
                       Structure.Chance (Planet_Category, Terrain_Id);
         begin
            if Chance > 0 then
               if WL.Random.Random_Number (1, Chance_Against) <= Chance then
                  return Structure;
               end if;
            end if;
         end;
      end loop;
      return null;
   end Random_Bonus;

end Carthage.Structures.Configure;
