with Ada.Strings.Fixed;

with Tropos.Reader;
with Tropos.Writer;

with Carthage.Configure;
with Carthage.Import;

package body Carthage.Worlds.Configure is

   Have_Terrain_Config   : Boolean := False;
   Terrain_Colour_Config : Tropos.Configuration;

   procedure Read_Terrain_Config;

   procedure Configure_Movement_Costs
     (Config : Tropos.Configuration)
   is
   begin
      for Terrain_Config of Config loop
         declare
            use type Carthage.Terrain.Terrain_Type;
            Terrain_Name : constant String :=
                             (if Terrain_Config.Config_Name = "arid"
                              then "arid_grass"
                              else Terrain_Config.Config_Name);
            Terrain      : constant Carthage.Terrain.Terrain_Type :=
                             (if Carthage.Terrain.Exists (Terrain_Name)
                              then Carthage.Terrain.Get (Terrain_Name)
                              elsif Terrain_Name = "road"
                              then null
                              else raise Constraint_Error with
                                "no such terrain in movement config: "
                              & Terrain_Name);
         begin
            for World_Config of Terrain_Config loop
               if Exists (World_Config.Config_Name) then
                  declare
                     use Carthage.Units;
                     World : constant World_Type :=
                               Get (World_Config.Config_Name);
                     Index    : Natural := 0;
                     Movement : Unit_Movement_Array;

                     procedure Set_Movement
                       (W : in out World_Record'Class);

                     ------------------
                     -- Set_Movement --
                     ------------------

                     procedure Set_Movement
                       (W : in out World_Record'Class)
                     is
                     begin
                        if Terrain = null then
                           W.Road_Movement := Movement;
                        else
                           declare
                              Current : Terrain_Info :=
                                          W.Terrain_Info.Element (Terrain);
                           begin
                              Current.Movement := Movement;
                              W.Terrain_Info.Replace_Element
                                (Terrain, Current);
                           end;
                        end if;
                     end Set_Movement;

                  begin
                     for Move_Config of World_Config loop
                        Movement (Unit_Category'Val (Index)) :=
                          Move_Config.Value;
                        Index := Index + 1;
                     end loop;

                     Db.Update (World.Reference, Set_Movement'Access);
                  end;
               end if;
            end loop;
         end;
      end loop;
   end Configure_Movement_Costs;

   ---------------------
   -- Configure_World --
   ---------------------

   procedure Configure_World
     (Config : Tropos.Configuration)
   is
      procedure Create (World : in out World_Class);

      ------------
      -- Create --
      ------------

      procedure Create (World : in out World_Class) is
         Terrain_Config : constant Tropos.Configuration :=
                            Config.Child ("terrain");
         Climate_Config : constant Tropos.Configuration :=
                            Config.Child ("climate-terrain");
      begin
         World.Create_With_Identity (Config.Config_Name);

         World.Index := Config.Get ("index");
         World.Smoothness := Config.Get ("smooth", 3);
         World.Base_Land := Carthage.Terrain.Get (Config.Get ("base-land"));
         World.Ave_Temperature :=
           Carthage.Climate.Temperature_Range
             (Float'(Config.Get ("temperature")));

         World.Terrain :=
           new Frequency_Array (1 .. Terrain_Config.Child_Count);

         for I in World.Terrain'Range loop
            World.Terrain (I) :=
              (Terrain =>
                 Carthage.Terrain.Get
                   (Terrain_Config.Child (I).Config_Name),
               Frequency =>
                 Terrain_Config.Child (I).Value);
         end loop;

         World.Climate_Terrain :=
           new Climate_Terrain_Array (1 .. Climate_Config.Child_Count);

         for I in World.Climate_Terrain'Range loop
            declare
               use Carthage.Climate;
               Cfg : constant Tropos.Configuration :=
                       Climate_Config.Child (I);
               Terrain_Name : constant String := Cfg.Config_Name;
               Terrain      : constant Carthage.Terrain.Terrain_Type :=
                                Carthage.Terrain.Get (Terrain_Name);
               Humid_Low    : constant Humidity_Range :=
                                (if Cfg.Contains ("humidity")
                                 then Humidity_Range
                                   (Float'(Cfg.Child ("humidity").Get (1))
                                    / 100.0)
                                 else Humidity_Range'First);
               Humid_High   : constant Humidity_Range :=
                                (if Cfg.Contains ("humidity")
                                 then Humidity_Range
                                   (Float'(Cfg.Child ("humidity").Get (2))
                                    / 100.0)
                                 else Humidity_Range'Last);
               Temp_Low    : constant Temperature_Range :=
                                (if Cfg.Contains ("temperature")
                                 then Temperature_Range
                                   (Float'(Cfg.Child ("temperature").Get (1)))
                                 else Temperature_Range'First);
               Temp_High   : constant Temperature_Range :=
                                (if Cfg.Contains ("temperature")
                                 then Temperature_Range
                                   (Float'(Cfg.Child ("temperature").Get (2)))
                                 else Temperature_Range'Last);
            begin
               World.Climate_Terrain (I) :=
                 Climate_Terrain'
                   (Terrain       => Terrain,
                    Humidity_Low  => Humid_Low,
                    Humidity_High => Humid_High,
                    Temp_Low      => Temp_Low,
                    Temp_High     => Temp_High);
            end;
         end loop;

      end Create;

   begin
      if not Have_Terrain_Config then
         Read_Terrain_Config;
         Tropos.Writer.Write_Config
           (Terrain_Colour_Config, "terrain-config.txt");
      end if;

      Db.Create (Create'Access);
   end Configure_World;

   -------------------------
   -- Read_Terrain_Config --
   -------------------------

   procedure Read_Terrain_Config is
      Eofs_Config : constant Tropos.Configuration :=
                      Tropos.Reader.Read_Config
                        (Carthage.Configure.Fading_Suns_Data_File
                           ("TERCOLOR"));
      Name        : Boolean := True;
      Name_Config : Tropos.Configuration;

      function To_Id (Name : String) return String;

      -----------
      -- To_Id --
      -----------

      function To_Id (Name : String) return String is
         Result : String := Name;
      begin
         for Ch of Result loop
            if Ch = ' ' then
               Ch := '_';
            end if;
         end loop;
         return Result;
      end To_Id;

   begin
      Terrain_Colour_Config := Tropos.New_Config ("terrain-colour");
      for Config of Eofs_Config.Child (1) loop
         if Name then
            Name_Config := Config;
         else
            declare
               Stat_Config : Tropos.Configuration :=
                               Tropos.New_Config
                                 (To_Id (Name_Config.Config_Name));
               Stats : Carthage.Import.Numeric_Settings (1 .. 5);
            begin
               Carthage.Import.Scan_Settings (Config.Config_Name, Stats);
               for Stat of Stats loop
                  Stat_Config.Add
                    (Tropos.New_Config
                       (Ada.Strings.Fixed.Trim
                            (Integer'Image (Stat),
                             Ada.Strings.Left)));
               end loop;
               Terrain_Colour_Config.Add (Stat_Config);
            end;
         end if;
         Name := not Name;
      end loop;

      Have_Terrain_Config := True;
   end Read_Terrain_Config;

end Carthage.Worlds.Configure;
