with WL.Random;

with Carthage.Calendar;
with Carthage.Logging;
with Carthage.Updates;

package body Carthage.Combat is

   Under_Damage : constant array (1 .. 5, 1 .. 10) of Positive :=
                    (1 => (10, 15, 20, 25, 30, 40, 50, 60, 70, 80),
                     2 => (5, 10, 15, 20, 25, 30, 40, 50, 60, 70),
                     3 => (1, 5, 10, 15, 20, 25, 30, 40, 50, 60),
                     4 => (1, 3, 5, 10, 15, 20, 25, 30, 40, 50),
                     5 => (1, 2, 3, 5, 10, 15, 20, 25, 30, 40));

   Over_Damage  : constant array (1 .. 8, 1 .. 10) of Positive :=
                    (1 => (10, 15, 20, 25, 30, 40, 50, 60, 70, 80),
                     2 => (15, 20, 25, 30, 40, 50, 60, 70, 80, 90),
                     3 => (20, 25, 30, 40, 50, 60, 70, 80, 90, 100),
                     4 => (25, 30, 40, 50, 60, 70, 80, 90, 100, 100),
                     5 => (30, 40, 50, 60, 70, 80, 90, 100, 100, 100),
                     6 => (40, 50, 60, 70, 80, 90, 100, 100, 100, 100),
                     7 => (50, 60, 70, 80, 90, 100, 100, 100, 100, 100),
                     8 => (60, 70, 80, 90, 100, 100, 100, 100, 100, 100));

   package Battle_Vectors is
     new Ada.Containers.Vectors (Positive, Battle_Record);

   Current_Battles : Battle_Vectors.Vector;

   procedure Resolve
     (Attack : in out Attack_Record);

   type Battle_Update is
     new Carthage.Updates.Update_Interface with
      record
         Battle_Index : Positive;
      end record;

   overriding procedure Activate
     (Upd : Battle_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Upd : Battle_Update)
   is
      Battle : Battle_Record renames Current_Battles (Upd.Battle_Index);
   begin
      if not Battle.Active then
         return;
      end if;

      Attacker (Battle).Log ("attacking " & Defender (Battle).Name);
      for Weapon in Carthage.Units.Weapon_Category loop
         declare
            Round : constant Carthage.Combat.Attack_Record_Array :=
                      Carthage.Combat.Attack_Round (Battle, Weapon);
         begin
            for Attack of Round loop
               Attacker (Battle).Log (Image (Attack));
            end loop;
         end;
      end loop;

      for Stack of Battle.Stacks loop
         Stack.Update.Remove_Dead_Assets;
      end loop;

      if Battle.Active then
         Carthage.Updates.Queue (Upd, Carthage.Calendar.Days (1));
      end if;

   end Activate;

   ---------------
   -- Add_Stack --
   ---------------

   procedure Add_Stack
     (Battle : in out Battle_Record;
      Stack  : Carthage.Stacks.Stack_Type)
   is
      use type Carthage.Houses.House_Type;
      Attacking : constant Boolean := Stack.Owner = Battle.Attacker;
      Defending : constant Boolean := Stack.Owner = Battle.Defender;
   begin
      if not Attacking and then not Defending then
         Stack.Log ("neutral during battle");
         return;
      elsif Attacking then
         Stack.Log ("attacking");
      else
         Stack.Log ("defending");
      end if;

      for I in 1 .. Stack.Count loop
         if Attacking then
            Stack.Asset (I).Log ("attacker asset");
            Battle.Attackers.Append (Stack.Asset (I));
         else
            Stack.Asset (I).Log ("defender asset");
            Battle.Defenders.Append (Stack.Asset (I));
         end if;
      end loop;

      Battle.Stacks.Append (Stack);

   end Add_Stack;

   ------------------
   -- Attack_Round --
   ------------------

   function Attack_Round
     (Battle : in out Battle_Record;
      Weapon : Carthage.Units.Weapon_Category)
      return Attack_Record_Array
   is
      Max    : constant Natural :=
                 Battle.Attackers.Last_Index
                   + Battle.Defenders.Last_Index;
      Result : Attack_Record_Array (1 .. Max);
      Count  : Natural := 0;

      procedure Check
        (Asset   : Carthage.Assets.Asset_Type;
         Targets : Asset_Vectors.Vector);

      function Choose_Target
        (Targets : Asset_Vectors.Vector)
         return Carthage.Assets.Asset_Type;

      -----------
      -- Check --
      -----------

      procedure Check
        (Asset   : Carthage.Assets.Asset_Type;
         Targets : Asset_Vectors.Vector)
      is
      begin
         if Asset.Unit.Has_Attack (Weapon) then
            declare
               use type Carthage.Assets.Asset_Type;
               use all type Carthage.Units.Weapon_Category;
               Defender : constant Carthage.Assets.Asset_Type :=
                            Choose_Target (Targets);
               Attack : Attack_Record;
            begin
               if Defender = null then
                  Asset.Log ("no targets");
                  return;
               end if;

               Attack := Attack_Record'
                 (Attacker        => Asset,
                  Defender        => Defender,
                  Weapon          => Weapon,
                  Accuracy        => Asset.Unit.Accuracy (Weapon),
                  Agility         => Defender.Unit.Agility,
                  Strength        => Asset.Unit.Strength (Weapon),
                  Armour          =>
                    (if Weapon = Psy
                     then Defender.Unit.Psychic_Defense
                     else Defender.Unit.Armour),
                  Hit_Roll        =>
                    WL.Random.Random_Number (1, 20),
                  Hit             => False,
                  Damage_Roll     =>
                    WL.Random.Random_Number (1, 10),
                  Damage          => 0);

               Resolve (Attack);

               Count := Count + 1;
               Result (Count) := Attack;
            end;
         end if;
      end Check;

      -------------------
      -- Choose_Target --
      -------------------

      function Choose_Target
        (Targets : Asset_Vectors.Vector)
         return Carthage.Assets.Asset_Type
      is
         Lowest : Natural := Natural'Last;
         Target : Carthage.Assets.Asset_Type := null;
      begin
         for Possible of Targets loop
            if Possible.Health > 0
              and then Possible.Unit.Rank < Lowest
            then
               Target := Possible;
               Lowest := Possible.Unit.Rank;
            end if;
         end loop;

         return Target;
      end Choose_Target;

   begin
      for Asset of Battle.Attackers loop
         if Asset.Alive then
            Check (Asset, Battle.Defenders);
         end if;
      end loop;
      for Asset of Battle.Defenders loop
         if Asset.Alive then
            Check (Asset, Battle.Attackers);
         end if;
      end loop;

      for I in 1 .. Count loop
         if Result (I).Defender.Alive
           and then Result (I).Damage > 0
         then
            Result (I).Defender.Update.Damage
              (Result (I).Damage);
            if not Result (I).Defender.Alive then
               Result (I).Defender.Log
                 ("destroyed by " & Result (I).Attacker.Identifier);
            end if;
         end if;
      end loop;

      return Result (1 .. Count);

   end Attack_Round;

   ------------
   -- Create --
   ------------

   procedure Create
     (Battle   : in out Battle_Record;
      Attacker : Carthage.Houses.House_Type;
      Defender : Carthage.Houses.House_Type)
   is
   begin
      Battle.Active := True;
      Battle.Attacker := Attacker;
      Battle.Defender := Defender;
      Battle.Attackers.Clear;
      Battle.Defenders.Clear;
   end Create;

   -----------
   -- Image --
   -----------

   function Image (Attack : Attack_Record) return String is
   begin
      return Attack.Attacker.Identifier & " vs "
        & Attack.Defender.Identifier
        & " using " & Attack.Weapon'Img
        & ": acc" & Attack.Accuracy'Img
        & " ag" & Attack.Agility'Img
        & " str" & Attack.Strength'Img
        & " arm" & Attack.Armour'Img
        & " roll" & Attack.Hit_Roll'Img
        & (if Attack.Hit
           then ": hit: dmg roll" & Attack.Damage_Roll'Img
           & " dmg" & Attack.Damage'Img
           else ": miss");
   end Image;

   ----------------
   -- New_Battle --
   ----------------

   procedure New_Battle
     (Attacker : Carthage.Stacks.Stack_Type;
      Defender : Carthage.Stacks.Stack_Type;
      Planet   : Carthage.Planets.Planet_Type;
      Tile     : Carthage.Tiles.Tile_Type)
   is
      Battle : Battle_Record;
   begin
      Create (Battle, Attacker.Owner, Defender.Owner);
      Battle.Planet := Planet;
      Battle.Tile := Tile;
      Add_Stack (Battle, Attacker);
      Add_Stack (Battle, Defender);
      Current_Battles.Append (Battle);
      Carthage.Updates.Queue
        (Battle_Update'(Battle_Index => Current_Battles.Last_Index),
         Carthage.Calendar.Days (1));
   end New_Battle;

   -------------
   -- Resolve --
   -------------

   procedure Resolve
     (Attack : in out Attack_Record)
   is
   begin
      Attack.Hit :=
        Attack.Hit_Roll + Attack.Accuracy - Attack.Agility
          >= 10;
      if Attack.Hit then
         declare
            Over : constant Positive :=
                     Natural'Min
                       (Natural'Max (Attack.Strength / Attack.Armour, 1),
                        8);
            Under : constant Positive :=
                      Natural'Min
                        (Natural'Max (Attack.Armour / Attack.Strength, 1),
                         5);
         begin
            if Over > 1 then
               Attack.Damage := Over_Damage (Over, Attack.Damage_Roll);
            else
               Attack.Damage := Under_Damage (Under, Attack.Damage_Roll);
            end if;
         end;
      end if;
   end Resolve;

   ------------------
   -- Scan_Battles --
   ------------------

   procedure Scan_Battles
     (Process : not null access
        procedure (Battle : in out Battle_Record))
   is
   begin
      for Battle of Current_Battles loop
         if Battle.Active then
            declare
               use type Carthage.Tiles.Tile_Type;
               Attackers : Natural := 0;
               Defenders : Natural := 0;
               Name      : constant String :=
                             "The Battle of " & Battle.Planet.Name
                             & (if Battle.Tile = null then ""
                                else " at " & Battle.Tile.Description);
            begin
               for Asset of Battle.Attackers loop
                  if Asset.Alive then
                     Attackers := Attackers + 1;
                  end if;
               end loop;
               for Asset of Battle.Defenders loop
                  if Asset.Alive then
                     Defenders := Defenders + 1;
                  end if;
               end loop;

               if Defenders = 0 then
                  if Attackers = 0 then
                     Battle.Attacker.Log
                       (Name & " vs " & Battle.Defender.Name
                        & ": mutual destruction");
                  else
                     Battle.Attacker.Log
                       (Name & ": victory against " & Battle.Defender.Name);
                  end if;
                  Battle.Active := False;
               elsif Attackers = 0 then
                  Battle.Defender.Log
                    (Name & ": victory against " & Battle.Attacker.Name);
                  Battle.Active := False;
               end if;
            end;
         end if;
      end loop;

      for Battle of Current_Battles loop
         if Battle.Active then
            declare
               use type Carthage.Tiles.Tile_Type;
               Name      : constant String :=
                             "The Battle of " & Battle.Planet.Name
                             & (if Battle.Tile = null then ""
                                else " at " & Battle.Tile.Description);
            begin
               Carthage.Logging.Log (Name);
            end;
            Process (Battle);
         end if;
      end loop;
   end Scan_Battles;

end Carthage.Combat;
