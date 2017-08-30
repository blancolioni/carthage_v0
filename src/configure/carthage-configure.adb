with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

with WL.Localisation;

with Tropos.Reader;
with Tropos.Writer;

with Carthage.Paths;

with Carthage.Structures.Configure;
with Carthage.Galaxy.Configure;
with Carthage.Houses.Configure;
with Carthage.Worlds.Configure;
with Carthage.Planets.Configure;
with Carthage.Resources.Configure;
with Carthage.Technology.Configure;
with Carthage.Terrain.Configure;
with Carthage.Units.Configure;

with Carthage.Import;
with Carthage.UI.Maps;

with Carthage.Options;

with Carthage.Import.Galaxy;

package body Carthage.Configure is

   Init_Config  : Tropos.Configuration;
   Eofs_Config  : Tropos.Configuration;

   function Scenario_File
     (Scenario_Name : String;
      File_Name     : String)
      return String
   is (Carthage.Paths.Config_File
       ("scenarios/" & Scenario_Name & "/" & File_Name));

   function Fading_Suns_Bin_File
     (Name : String)
      return String
   is (Eofs_Config.Get ("path") & "/BIN/" & Name & ".BIN");

   function Fading_Suns_Data_File
     (Name : String)
      return String
   is (Eofs_Config.Get ("path") & "/DAT/" & Name & ".DAT");

   function Fading_Suns_FLC_File
     (Name : String)
      return String
   is (Eofs_Config.Get ("path") & "/FLC/" & Name & ".FLC");

   function Fading_Suns_Rand_File
     (Name : String)
      return String
   is (Eofs_Config.Get ("path") & "/RAND/" & Name & ".TXT");

   procedure Load_Directory_Configuration
     (Directory_Name : String;
      Configure      : not null access
        procedure (Config : Tropos.Configuration));

   procedure Load_Scenario_Configuration
     (Scenario_Name : String;
      File_Name     : String;
      Configure     : not null access
        procedure (Config : Tropos.Configuration));

   procedure Load_Localisation
     (File : Ada.Directories.Directory_Entry_Type);

   procedure Import_Cities
     (Config : Tropos.Configuration);

   procedure Import_Technology
     (Config : Tropos.Configuration);

   procedure Import_Terrain
     (Colour_Config      : Tropos.Configuration;
      Road_Cost_Config   : Tropos.Configuration;
      Move_Cost_Config   : Tropos.Configuration);

   procedure Import_Units
     (Config : Tropos.Configuration);

   function To_Carthage_Id
     (Dat_Identifier   : String;
      Space_Substitute : Character := '-')
      return String;

   -------------------
   -- Import_Cities --
   -------------------

   procedure Import_Cities
     (Config : Tropos.Configuration)
   is

      Index : Positive := 1;

      At_Name  : Boolean := False;
      At_Stats : Boolean := False;

      Name_Config : Tropos.Configuration;

      Normal_Bonus : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Fading_Suns_Rand_File ("TYPE0"));

      City_Bonus : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Fading_Suns_Rand_File ("TYPE1"));

      Jungle_Bonus   : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Fading_Suns_Rand_File ("TYPE2"));

      Frozen_Bonus   : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Fading_Suns_Rand_File ("TYPE3"));

      Barren_Bonus   : constant Tropos.Configuration :=
                         Tropos.Reader.Read_Config
                           (Fading_Suns_Rand_File ("TYPE4"));

      procedure Create_Bonus_Config
        (Base        : Tropos.Configuration;
         Base_Name   : String;
         Id_Number   : Natural;
         Target      : in out Tropos.Configuration;
         Target_Name : String;
         Index       : Natural);

      procedure Load_Bonus_Table
        (Structure_Config : in out Tropos.Configuration);

      -------------------------
      -- Create_Bonus_Config --
      -------------------------

      procedure Create_Bonus_Config
        (Base        : Tropos.Configuration;
         Base_Name   : String;
         Id_Number   : Natural;
         Target      : in out Tropos.Configuration;
         Target_Name : String;
         Index       : Natural)
      is
         Child_Index : Natural := 0;
         Last_Child  : constant Natural := Base.Child_Count - 1;
         Skip        : Boolean := False;
         Odds        : Natural := 0;
      begin
         for Config of Base loop
            Child_Index := Child_Index + 1;
            if Child_Index in 2 .. Last_Child then
               declare
                  Local_Index : constant Natural := (Child_Index - 2) mod 3;
                  Local_Value : constant Natural :=
                                  Natural'Value (Config.Config_Name);

               begin
                  if Local_Index = 0 then
                     Skip := Local_Value + 1 /= Id_Number;
                  elsif not Skip then
                     if Local_Index = 1 then
                        Odds := Local_Value;
                     elsif Local_Value = Index then
                        Target.Add
                          (Base_Name & "-" & Target_Name,
                           Odds);
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end Create_Bonus_Config;

      ----------------------
      -- Load_Bonus_Table --
      ----------------------

      procedure Load_Bonus_Table
        (Structure_Config : in out Tropos.Configuration)
      is

         Bonus_Config : Tropos.Configuration :=
                          Tropos.New_Config ("bonus");

         procedure Load (Source : Tropos.Configuration;
                         Name   : String);

         ----------
         -- Load --
         ----------

         procedure Load (Source : Tropos.Configuration;
                         Name   : String)
         is
         begin
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "ocean", 0);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "grass", 1);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "arid_grass", 2);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "desert", 3);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "ice", 4);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "tundra", 5);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "mountain", 6);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "hill", 7);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "tree", 8);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "river", 9);
         end Load;

      begin
         Load (Normal_Bonus, "normal");
         Load (City_Bonus, "city");
         Load (Jungle_Bonus, "jungle");
         Load (Frozen_Bonus, "frozen");
         Load (Barren_Bonus, "barren");
         if Bonus_Config.Child_Count > 0 then
            Structure_Config.Add (Bonus_Config);
         end if;
      end Load_Bonus_Table;

   begin
      for Field_Config of Config.Child (1) loop
         declare
            Field : constant String := Field_Config.Config_Name;
         begin
            if Field = "name" then
               At_Name := True;
            elsif Field = "stats" then
               At_Stats := True;
            elsif At_Name then
               Name_Config := Field_Config;
               At_Name := False;
            elsif At_Stats then

               At_Stats := False;

               declare
                  Id          : constant String :=
                                  To_Carthage_Id (Name_Config.Config_Name);
                  Stats       : constant String := Field;
                  Output      : Tropos.Configuration :=
                                  Tropos.New_Config (Id);
                  Start_Index : Positive := Stats'First;

                  function Next_Number return Natural;

                  procedure Flag
                    (Name  : String;
                     Value : Natural);

                  procedure Number
                    (Name  : String;
                     Value : Natural);

                  ----------
                  -- Flag --
                  ----------

                  procedure Flag
                    (Name  : String;
                     Value : Natural)
                  is
                  begin
                     if Value /= 0 then
                        Output.Add (Name, "yes");
                     end if;
                  end Flag;

                  -----------------
                  -- Next_Number --
                  -----------------

                  function Next_Number return Natural is
                     Start : Positive;
                  begin
                     while Start_Index <= Stats'Last
                       and then not Ada.Characters.Handling.Is_Digit
                         (Stats (Start_Index))
                     loop
                        Start_Index := Start_Index + 1;
                     end loop;

                     Start := Start_Index;

                     while Start_Index <= Stats'Last
                       and then Ada.Characters.Handling.Is_Digit
                         (Stats (Start_Index))
                     loop
                        Start_Index := Start_Index + 1;
                     end loop;

                     return Natural'Value (Stats (Start .. Start_Index - 1));
                  end Next_Number;

                  ------------
                  -- Number --
                  ------------

                  procedure Number
                    (Name  : String;
                     Value : Natural)
                  is
                  begin
                     if Value /= 0 then
                        Output.Add (Name, Value);
                     end if;
                  end Number;

                  Water       : constant Natural := Next_Number;
                  Land        : constant Natural := Next_Number;
                  Road        : constant Natural := Next_Number;
                  Barren      : constant Natural := Next_Number;
                  Neutral     : constant Natural := Next_Number;
                  Build       : constant Natural := Next_Number;
                  Area        : constant Natural := Next_Number;
                  Maintenance : constant Natural := Next_Number;
                  Cost        : constant Natural := Next_Number;
                  Build_Time  : constant Natural := Next_Number;
                  Enabled_By  : constant Natural := Next_Number;
                  Value       : constant Natural := Next_Number;

               begin
                  Output.Add ("index", Index);

                  if Id = "farm"
                    or else Id = "mine"
                    or else Id = "well"
                    or else Id = "aborium"
                  then
                     Output.Add ("harvester", "yes");
                     Output.Add ("production", Id);
                  end if;

                  Flag ("water", Water);
                  Flag ("land", Land);
                  Flag ("road", Road);
                  Flag ("barren", Barren);
                  Flag ("neutral", Neutral);
                  Flag ("build", Build);

                  Number ("area", Area);
                  Number ("maintenance", Maintenance);
                  Number ("cost", Cost);
                  Number ("build-time", Build_Time);
                  Number ("value", Value);

                  if Enabled_By /= 0 then
                     Output.Add ("enabled-by",
                                 Carthage.Technology.Get
                                   (Enabled_By).Identifier);
                  end if;

                  Load_Bonus_Table (Output);

                  Tropos.Writer.Write_Config
                    (Output,
                     Carthage.Paths.Config_File
                       ("cities/" & Output.Config_Name & ".txt"));
                  Index := Index + 1;
               end;
            end if;
         end;
      end loop;
   end Import_Cities;

   -----------------------
   -- Import_Technology --
   -----------------------

   procedure Import_Technology
     (Config : Tropos.Configuration)
   is

      Index : Natural := 0;

      At_Name  : Boolean := False;
      At_Stats : Boolean := False;

      Name_Config : Tropos.Configuration;

   begin
      for Field_Config of Config.Child (1) loop
         declare
            Field : constant String := Field_Config.Config_Name;
         begin
            if Field = "name" then
               At_Name := True;
            elsif Field = "stats" then
               At_Stats := True;
            elsif At_Name then
               Name_Config := Field_Config;
               At_Name := False;
            elsif At_Stats then

               At_Stats := False;

               declare
                  Id          : constant String :=
                                  To_Carthage_Id (Name_Config.Config_Name);
                  Stats       : constant String := Field;
                  Output      : Tropos.Configuration :=
                                  Tropos.New_Config (Id);
                  Start_Index : Positive := Stats'First;

                  function Next_Number return Natural;

                  -----------------
                  -- Next_Number --
                  -----------------

                  function Next_Number return Natural is
                     Start : Positive;
                  begin
                     while Start_Index <= Stats'Last
                       and then not Ada.Characters.Handling.Is_Digit
                         (Stats (Start_Index))
                     loop
                        Start_Index := Start_Index + 1;
                     end loop;

                     Start := Start_Index;

                     while Start_Index <= Stats'Last
                       and then Ada.Characters.Handling.Is_Digit
                         (Stats (Start_Index))
                     loop
                        Start_Index := Start_Index + 1;
                     end loop;

                     return Natural'Value (Stats (Start .. Start_Index - 1));
                  end Next_Number;

                  T1         : constant Natural := Next_Number;
                  T2         : constant Natural := Next_Number;
                  T3         : constant Natural := Next_Number;
                  Cost       : constant Natural := Next_Number;
                  Like       : constant Natural := Next_Number;
               begin
                  Output.Add ("index", Index);
                  if T1 /= 0 then
                     Output.Add ("enabled-by", T1);
                  end if;
                  if T2 /= 0 then
                     Output.Add ("enabled-by", T2);
                  end if;
                  if T3 /= 0 then
                     Output.Add ("enabled-by", T3);
                  end if;
                  Output.Add ("cost", Cost);
                  Output.Add ("like", Like);

                  Tropos.Writer.Write_Config
                    (Output,
                     Carthage.Paths.Config_File
                       ("technology/" & Output.Config_Name & ".txt"));
                  Index := Index + 1;
               end;
            end if;
         end;
      end loop;
   end Import_Technology;

   --------------------
   -- Import_Terrain --
   --------------------

   procedure Import_Terrain
     (Colour_Config      : Tropos.Configuration;
      Road_Cost_Config   : Tropos.Configuration;
      Move_Cost_Config   : Tropos.Configuration)
   is
      pragma Unreferenced (Move_Cost_Config);
      Output : array (1 .. Colour_Config.Child (1).Child_Count / 2)
        of Tropos.Configuration;

      procedure Set_World_Settings
        (Config   : in out Tropos.Configuration;
         Name     : String;
         Source   : String);

      ------------------------
      -- Set_World_Settings --
      ------------------------

      procedure Set_World_Settings
        (Config   : in out Tropos.Configuration;
         Name     : String;
         Source   : String)
      is

         Settings : Carthage.Import.Numeric_Settings (1 .. 5);
         Out_Config : Tropos.Configuration :=
                        Tropos.New_Config (Name);
      begin
         Carthage.Import.Scan_Settings (Source, Settings);
         Out_Config.Add ("normal", Settings (1));
         Out_Config.Add ("city", Settings (2));
         Out_Config.Add ("ice", Settings (3));
         Out_Config.Add ("jungle", Settings (4));
         Out_Config.Add ("barren", Settings (5));
         Out_Config.Add ("desert", Settings (5));
         Config.Add (Out_Config);
      end Set_World_Settings;

   begin
      for I in Output'Range loop
         Output (I) :=
           Tropos.New_Config
             (To_Carthage_Id
                (Colour_Config.Child (1).Get (I * 2 - 1),
                 Space_Substitute => '_'));
         pragma Assert
           (String'(Colour_Config.Child (1).Get (I * 2 - 1))
              = Road_Cost_Config.Child (1).Get (I * 2 - 1));
         Set_World_Settings
           (Output (I), "colours",
            Colour_Config.Child (1).Get (I * 2));
         Set_World_Settings
           (Output (I), "road-cost",
            Road_Cost_Config.Child (1).Get (I * 2));
      end loop;

      for Config of Output loop
         if Config.Config_Name /= "road" then
            Tropos.Writer.Write_Config
              (Config,
               Carthage.Paths.Config_File
                 ("terrain/" & Config.Config_Name & ".txt"));
         end if;
      end loop;
   end Import_Terrain;

   ------------------
   -- Import_Units --
   ------------------

   procedure Import_Units
     (Config : Tropos.Configuration)
   is

      Index     : Natural := 0;
      Sub_Index : Natural;

      At_Index  : Boolean := False;
      At_Name   : Boolean := False;
      At_Abbrev : Boolean := False;
      At_Stats  : Boolean := False;
      At_Art    : Boolean := False;

      Name_Config   : Tropos.Configuration with Unreferenced;
      Abbrev_Config : Tropos.Configuration;
      Output        : Tropos.Configuration;
   begin
      for Unit_Config of Config loop

         At_Index := True;
         Index := Index + 1;
         Sub_Index := 0;

         for Field_Config of Unit_Config loop
            declare
               Field : constant String := Field_Config.Config_Name;
            begin
               if Field = "name" then
                  At_Name := True;
               elsif Field = "abbrev" then
                  At_Abbrev := True;
               elsif Field = "stats" then
                  At_Stats := True;
               elsif Field = "art" then
                  At_Art := True;
               elsif At_Index then
                  Index := Natural'Value (Field);
                  At_Index := False;
               elsif At_Name then
                  Name_Config := Field_Config;
                  At_Name := False;
               elsif At_Abbrev then
                  Abbrev_Config := Field_Config;
                  At_Abbrev := False;
               elsif At_Art then
                  Output.Add ("icon", Ada.Directories.Base_Name (Field));
                  Tropos.Writer.Write_Config
                    (Output,
                     Carthage.Paths.Config_File
                       ("units/" & Output.Config_Name & ".txt"));
                  At_Art := False;
               elsif At_Stats then

                  At_Stats := False;
                  Sub_Index := Sub_Index + 1;

                  declare
                     Id          : constant String :=
                                     To_Carthage_Id
                                       (Abbrev_Config.Config_Name)
                                     & Integer'Image (-Index)
                                     & (if Sub_Index > 1
                                        then Integer'Image (-Sub_Index)
                                        else "");
                     Stats       : constant String := Field;
                     Start_Index : Positive := Stats'First;

                     function Next_Name return String;

                     function Next_Number return Integer
                     is (Integer'Value (Next_Name));

                     ---------------
                     -- Next_Name --
                     ---------------

                     function Next_Name return String is
                        Start : Positive;
                     begin
                        while Start_Index <= Stats'Last
                          and then Stats (Start_Index) = ' '
                        loop
                           Start_Index := Start_Index + 1;
                        end loop;

                        Start := Start_Index;

                        while Start_Index <= Stats'Last
                          and then Stats (Start_Index) /= ' '
                        loop
                           Start_Index := Start_Index + 1;
                        end loop;

                        return Stats (Start .. Start_Index - 1);

                     end Next_Name;

                     Category              : constant String := Next_Name;
                     Move                  : constant Natural := Next_Number;
                     Spot                  : constant Natural := Next_Number;
                     Camo                  : constant Natural := Next_Number;
                     Ag                    : constant Natural := Next_Number;
                     Armour                : constant Natural := Next_Number;
                     Psydef                : constant Natural := Next_Number;
                     Water_Attack          : constant Natural := Next_Number;
                     Water_Strength        : constant Natural := Next_Number;
                     Indirect_Attack       : constant Natural := Next_Number;
                     Indirect_Strength     : constant Natural := Next_Number;
                     Air_Attack            : constant Natural := Next_Number;
                     Air_Strength          : constant Natural := Next_Number;
                     Direct_Attack         : constant Natural := Next_Number;
                     Direct_Strength       : constant Natural := Next_Number;
                     Close_Attack          : constant Natural := Next_Number;
                     Close_Strength        : constant Natural := Next_Number;
                     Psy_Attack            : constant Natural := Next_Number;
                     Psy_Strength          : constant Natural := Next_Number;
                     Range_Space_Attack    : constant Natural := Next_Number;
                     Range_Space_Strength  : constant Natural := Next_Number;
                     Direct_Space_Attack   : constant Natural := Next_Number;
                     Direct_Space_Strength : constant Natural := Next_Number;
                     Close_Space_Attack    : constant Natural := Next_Number;
                     Close_Space_Strength  : constant Natural := Next_Number;
                     Cargo                 : constant Natural := Next_Number;
                     Can_Be_Cargo          : constant Natural := Next_Number;
                     Non_Combat            : constant Natural := Next_Number;
                     Maintenance           : constant Natural := Next_Number;
                     Credit_Cost           : constant Natural := Next_Number;
                     Food_Cost             : constant Natural := Next_Number;
                     Energy_Cost           : constant Natural := Next_Number;
                     Metal_Cost            : constant Natural := Next_Number;
                     Trace_Cost            : constant Natural := Next_Number;
                     Exotic_Cost           : constant Natural := Next_Number;
                     Chems_Cost            : constant Natural := Next_Number;
                     Bio_Cost              : constant Natural := Next_Number;
                     Elec_Cost             : constant Natural := Next_Number;
                     CSteel_Cost           : constant Natural := Next_Number;
                     Wet_Cost              : constant Natural := Next_Number;
                     Mono_Cost             : constant Natural := Next_Number;
                     Gems_Cost             : constant Natural := Next_Number;
                     Sing_Cost             : constant Natural := Next_Number;
                     Required_Unit         : constant Integer := Next_Number;
                     Required_Building     : constant Integer := Next_Number;
                     Turns_To_Build        : constant Natural := Next_Number;
                     Tech_1                : constant Natural := Next_Number;
                     Tech_2                : constant Natural := Next_Number;
                     Tech_3                : constant Natural := Next_Number;
                     Tech_4                : constant Natural := Next_Number;
                  begin
                     Output := Tropos.New_Config (Id);
                     Output.Add ("index", Index);
                     Output.Add ("category", Category);
                     Output.Add ("move", Move);
                     Output.Add ("spot", Spot);
                     Output.Add ("camouflage", Camo);
                     Output.Add ("agility", Ag);
                     Output.Add ("armour", Armour);
                     Output.Add ("psy-defence", Psydef);

                     declare
                        Attacks : Tropos.Configuration :=
                                    Tropos.New_Config ("attack");

                        procedure Add_Attack
                          (Name     : String;
                           Accuracy : Natural;
                           Strength : Natural);

                        ----------------
                        -- Add_Attack --
                        ----------------

                        procedure Add_Attack
                          (Name     : String;
                           Accuracy : Natural;
                           Strength : Natural)
                        is
                        begin
                           if Accuracy > 0 then
                              declare
                                 Attack : Tropos.Configuration :=
                                            Tropos.New_Config (Name);
                              begin
                                 Attack.Add ("accuracy", Accuracy);
                                 Attack.Add ("strength", Strength);
                                 Attacks.Add (Attack);
                              end;
                           end if;
                        end Add_Attack;

                     begin
                        Add_Attack
                          ("water", Water_Attack, Water_Strength);
                        Add_Attack
                          ("indirect", Indirect_Attack, Indirect_Strength);
                        Add_Attack
                          ("air", Air_Attack, Air_Strength);
                        Add_Attack
                          ("direct", Direct_Attack, Direct_Strength);
                        Add_Attack
                          ("close", Close_Attack, Close_Strength);
                        Add_Attack
                          ("psy", Psy_Attack, Psy_Strength);
                        Add_Attack
                          ("ranged-space",
                           Range_Space_Attack, Range_Space_Strength);
                        Add_Attack
                          ("direct-space",
                           Direct_Space_Attack, Direct_Space_Strength);
                        Add_Attack
                          ("close-space",
                           Close_Space_Attack, Close_Space_Strength);

                        Output.Add (Attacks);

                     end;

                     if Cargo > 0 then
                        Output.Add ("cargo", Cargo);
                     end if;

                     if Can_Be_Cargo /= 0 then
                        Output.Add ("can-be-cargo", "yes");
                     end if;

                     if Non_Combat = 0 then
                        Output.Add ("combat", "yes");
                     end if;

                     Output.Add ("maintenance", Maintenance);
                     Output.Add ("credit-cost", Credit_Cost);

                     declare
                        Resources : Tropos.Configuration :=
                                      Tropos.New_Config ("resources");

                        procedure Add_Resource
                          (Name     : String;
                           Quantity : Natural);

                        procedure Add_Resource
                          (Name     : String;
                           Quantity : Natural)
                        is
                        begin
                           if Quantity > 0 then
                              Resources.Add (Name, Quantity);
                           end if;
                        end Add_Resource;

                     begin
                        Add_Resource ("food", Food_Cost);
                        Add_Resource ("energy", Energy_Cost);
                        Add_Resource ("metal", Metal_Cost);
                        Add_Resource ("trace", Trace_Cost);
                        Add_Resource ("exotics", Exotic_Cost);
                        Add_Resource ("chemicals", Chems_Cost);
                        Add_Resource ("bio-chemicals", Bio_Cost);
                        Add_Resource ("electronics", Elec_Cost);
                        Add_Resource ("ceramsteel", CSteel_Cost);
                        Add_Resource ("wetware", Wet_Cost);
                        Add_Resource ("monopoles", Mono_Cost);
                        Add_Resource ("gems", Gems_Cost);
                        Add_Resource ("singularities", Sing_Cost);

                        if Resources.Child_Count > 0 then
                           Output.Add (Resources);
                        end if;

                     end;

                     if Required_Unit >= 0 then
                        Output.Add ("required-unit", Required_Unit);
                     end if;

                     if Required_Building >= 0 then
                        Output.Add ("required-building", Required_Building);
                     end if;

                     Output.Add ("turns-to-build", Turns_To_Build);

                     declare
                        Required_Tech : Tropos.Configuration;
                     begin
                        if Tech_1 /= 0 then
                           Required_Tech.Add ("tec", Tech_1);
                        end if;
                        if Tech_2 /= 0 then
                           Required_Tech.Add ("tec", Tech_2);
                        end if;
                        if Tech_3 /= 0 then
                           Required_Tech.Add ("tec", Tech_3);
                        end if;
                        if Tech_4 /= 0 then
                           Required_Tech.Add ("tec", Tech_4);
                        end if;
                     end;

                  end;
               end if;
            end;
         end loop;
      end loop;
   end Import_Units;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration is
   begin
      if Carthage.Options.Clear_Import_Cache then
         Ada.Text_IO.Put_Line ("option: clear import cache = yes");
      end if;

      Ada.Text_IO.Put_Line ("loading configuration ...");
      Init_Config :=
        Tropos.Reader.Read_Config
          (Carthage.Paths.Config_File ("init.txt"));

      if not Ada.Directories.Exists
        (Carthage.Paths.Config_File ("eofs.txt"))
      then
         Eofs_Config := Tropos.New_Config ("eofs");
         Eofs_Config.Add
           ("path",
            Init_Config.Get
              ("eofs-path",
               Carthage.Paths.Config_File ("eofs/standard")));
         Tropos.Writer.Write_Config
           (Eofs_Config, Carthage.Paths.Config_File ("eofs.txt"));
      else
         Eofs_Config :=
           Tropos.Reader.Read_Config
             (Carthage.Paths.Config_File ("eofs.txt"));
      end if;

      Ada.Text_IO.Put_Line ("  localisation");
      declare
         use all type Ada.Directories.File_Kind;
         Path : constant String :=
                  Carthage.Paths.Config_File
                    ("localisation/"
                     & Init_Config.Get ("language", "english"));
      begin
         Ada.Directories.Search
           (Directory => Path,
            Pattern   => "*",
            Filter    => (Ordinary_File => True, others => False),
            Process   => Load_Localisation'Access);
      end;

      Ada.Text_IO.Put_Line ("  surface graph");
      Carthage.Planets.Configure.Create_Surface_Graph;

      declare
         Technology_Path : constant String :=
                             Carthage.Paths.Config_File
                               ("technology");
      begin
         if not Ada.Directories.Exists (Technology_Path) then
            Ada.Directories.Create_Directory (Technology_Path);
         end if;

         if Carthage.Options.Clear_Import_Cache
           or else not Ada.Directories.Exists
             (Technology_Path & "/nothing.txt")
         then
            Ada.Text_IO.Put_Line ("  importing fading suns tech");
            Import_Technology
              (Tropos.Reader.Read_Config
                 (Fading_Suns_Data_File ("TECH")));
         end if;
      end;

      Ada.Text_IO.Put_Line ("  technology");
      Load_Directory_Configuration
        ("technology",
         Carthage.Technology.Configure.Configure_Technology'Access);
      Load_Directory_Configuration
        ("technology",
         Carthage.Technology.Configure.Configure_Tree'Access);

      Ada.Text_IO.Put_Line ("  resources");
      Load_Directory_Configuration
        ("resources", Carthage.Resources.Configure.Configure_Resource'Access);

      declare
         Terrain_Path : constant String :=
                          Carthage.Paths.Config_File
                            ("terrain");
      begin
         if not Ada.Directories.Exists (Terrain_Path) then
            Ada.Directories.Create_Directory (Terrain_Path);
         end if;

         if Carthage.Options.Clear_Import_Cache
           or else not Ada.Directories.Exists
             (Terrain_Path & "/grass.txt")
         then
            Ada.Text_IO.Put_Line ("  importing fading suns terrain");
            Import_Terrain
              (Colour_Config    =>
                 Tropos.Reader.Read_Config
                   (Fading_Suns_Data_File ("TERCOLOR")),
               Road_Cost_Config =>
                 Tropos.Reader.Read_Config
                   (Fading_Suns_Data_File ("ROADCOST")),
               Move_Cost_Config =>
                 Tropos.Reader.Read_Config
                   (Fading_Suns_Data_File ("TERRCOST")));
         end if;
      end;

      Ada.Text_IO.Put_Line ("  terrain");
      Load_Directory_Configuration
        ("terrain", Carthage.Terrain.Configure.Configure_Terrain'Access);

      Ada.Text_IO.Put_Line ("  planet categories");
      Load_Directory_Configuration
        ("worlds",
         Carthage.Worlds.Configure.Configure_World'Access);

      declare
         Structure_Path : constant String :=
                            Carthage.Paths.Config_File
                              ("cities");
      begin
         if not Ada.Directories.Exists (Structure_Path) then
            Ada.Directories.Create_Directory (Structure_Path);
         end if;

         if Carthage.Options.Clear_Import_Cache
           or else not Ada.Directories.Exists
             (Structure_Path & "/palace.txt")
         then
            Ada.Text_IO.Put_Line ("  importing fading suns structures");
            Import_Cities
              (Tropos.Reader.Read_Config
                 (Fading_Suns_Data_File ("STRBUILD")));
         end if;
      end;

      Ada.Text_IO.Put_Line ("  structures");
      Load_Directory_Configuration
        ("cities",
         Carthage.Structures.Configure.Configure_Structure'Access);

      Carthage.Structures.Configure.Configure_Bonus_Production
        (Tropos.Reader.Read_Config
           (Carthage.Paths.Config_File
                ("bonus/bonus.txt")));

      declare
         Unit_Path : constant String :=
                       Carthage.Paths.Config_File
                         ("units");
      begin
         if not Ada.Directories.Exists (Unit_Path) then
            Ada.Directories.Create_Directory (Unit_Path);
         end if;

         if Carthage.Options.Clear_Import_Cache
           or else not Ada.Directories.Exists
             (Unit_Path & "/noble.txt")
         then
            Ada.Text_IO.Put_Line ("  importing fading suns units");
            Import_Units
              (Tropos.Reader.Read_Config
                 (Fading_Suns_Data_File ("UNIT")));
         end if;
      end;

      Ada.Text_IO.Put_Line ("  units");
      Load_Directory_Configuration
        ("units", Carthage.Units.Configure.Configure_Unit'Access);

      Ada.Text_IO.Put_Line ("  tiles");
      Carthage.UI.Maps.Configure_Tile_Resources
        (Tropos.Reader.Read_Config
           (Carthage.Paths.Config_File
                ("ui/tilemap.txt")));

   end Load_Configuration;

   ----------------------------------
   -- Load_Directory_Configuration --
   ----------------------------------

   procedure Load_Directory_Configuration
     (Directory_Name : String;
      Configure      : not null access
        procedure (Config : Tropos.Configuration))
   is
   begin
      Tropos.Reader.Read_Config
        (Path      => Carthage.Paths.Config_File (Directory_Name),
         Extension => "txt",
         Configure => Configure);
   end Load_Directory_Configuration;

   -------------------------------
   -- Load_Fading_Suns_Scenario --
   -------------------------------

   procedure Load_Fading_Suns_Scenario is
   begin
      Carthage.Import.Galaxy.Import_Galaxy
        (Eofs_Config.Get ("path") & "/GALAXY.GAL");
   end Load_Fading_Suns_Scenario;

   -----------------------
   -- Load_Localisation --
   -----------------------

   procedure Load_Localisation
     (File : Ada.Directories.Directory_Entry_Type)
   is
   begin
      WL.Localisation.Read_Localisation
        (Ada.Directories.Full_Name (File));
   end Load_Localisation;

   -------------------
   -- Load_Scenario --
   -------------------

   procedure Load_Scenario (Name : String) is

      Start_Config : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Scenario_File (Name, "start.txt"));

      procedure Configure_House_Start
        (House : Carthage.Houses.House_Type);

      ---------------------------
      -- Configure_House_Start --
      ---------------------------

      procedure Configure_House_Start
        (House : Carthage.Houses.House_Type)
      is
         Category_Name : constant String :=
                           Ada.Characters.Handling.To_Lower
                             (Carthage.Houses.House_Category'Image
                                (House.Category));
      begin
         if Start_Config.Contains (Category_Name) then
            declare
               Config : constant Tropos.Configuration :=
                          Start_Config.Child (Category_Name);
            begin
               Carthage.Houses.Configure.Configure_Start (House, Config);
            end;
         end if;
      end Configure_House_Start;

   begin
      Ada.Text_IO.Put_Line ("loading scenario: " & Name);

      Ada.Text_IO.Put_Line ("  planets");
      Load_Directory_Configuration
        ("scenarios/" & Name & "/planets",
         Carthage.Planets.Configure.Configure_Planet'Access);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("  houses");
      Load_Directory_Configuration
        ("scenarios/" & Name & "/houses",
          Carthage.Houses.Configure.Configure_House'Access);

      Ada.Text_IO.Put_Line ("  gates");
      Carthage.Galaxy.Configure.Configure_Gates
        (Tropos.Reader.Read_Config
           (Carthage.Paths.Config_File
                ("scenarios/" & Name & "/gates.txt")));

      Ada.Text_IO.Put_Line ("  star map");
      Load_Scenario_Configuration
        (Name, "starmap",
         Carthage.Galaxy.Configure.Configure_Positions'Access);

      Ada.Text_IO.Put_Line ("  houses");
      Carthage.Houses.Scan
        (Configure_House_Start'Access);
   end Load_Scenario;

   ---------------------------------
   -- Load_Scenario_Configuration --
   ---------------------------------

   procedure Load_Scenario_Configuration
     (Scenario_Name : String;
      File_Name     : String;
      Configure     : not null access
        procedure (Config : Tropos.Configuration))
   is
   begin
      Configure
        (Tropos.Reader.Read_Config
           (Carthage.Paths.Config_File
                ("scenarios/" & Scenario_Name & "/" & File_Name & ".txt")));
   end Load_Scenario_Configuration;

   --------------------------
   -- Load_Standard_Houses --
   --------------------------

   procedure Load_Standard_Houses is
   begin
      Load_Directory_Configuration
        (Directory_Name => "scenarios/standard/houses",
         Configure      =>
           Carthage.Houses.Configure.Configure_House'Access);
   end Load_Standard_Houses;

   --------------------
   -- To_Carthage_Id --
   --------------------

   function To_Carthage_Id
     (Dat_Identifier   : String;
      Space_Substitute : Character := '-')
      return String
   is
      Result : String := Dat_Identifier;
   begin
      for Ch of Result loop
         if Ada.Characters.Handling.Is_Upper (Ch) then
            Ch := Ada.Characters.Handling.To_Lower (Ch);
         elsif Ch = ' ' then
            Ch := Space_Substitute;
         end if;
      end loop;
      return Result;
   end To_Carthage_Id;

end Carthage.Configure;
