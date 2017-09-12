private with Ada.Containers.Vectors;

with Carthage.Assets;
with Carthage.Houses;
with Carthage.Stacks;
with Carthage.Units;

package Carthage.Combat is

   procedure New_Battle
     (Attacker : Carthage.Stacks.Stack_Type;
      Defender : Carthage.Stacks.Stack_Type);

   type Attack_Record is private;

   function Image (Attack : Attack_Record) return String;

   function Attacker
     (Attack : Attack_Record)
      return Carthage.Assets.Asset_Type;

   function Defender
     (Attack : Attack_Record)
      return Carthage.Assets.Asset_Type;

   function Weapon
     (Attack : Attack_Record)
      return Carthage.Units.Weapon_Category;

   function Attack_Accuracy
     (Attack : Attack_Record)
      return Natural;

   function Defense_Agility
     (Attack : Attack_Record)
      return Natural;

   function Attack_Strength
     (Attack : Attack_Record)
      return Natural;

   function Defense_Strength
     (Attack : Attack_Record)
      return Natural;

   function Hit_Roll
     (Attack : Attack_Record)
      return Positive;

   function Hit
     (Attack : Attack_Record)
      return Boolean;

   function Damage_Roll
     (Attack : Attack_Record)
      return Positive;

   function Damage
     (Attack : Attack_Record)
      return Natural;

   type Battle_Record is private;

   procedure Create
     (Battle   : in out Battle_Record;
      Attacker : Carthage.Houses.House_Type;
      Defender : Carthage.Houses.House_Type);

   function Attacker
     (Battle : Battle_Record)
      return Carthage.Houses.House_Type;

   function Defender
     (Battle : Battle_Record)
      return Carthage.Houses.House_Type;

   procedure Add_Stack
     (Battle : in out Battle_Record;
      Stack  : Carthage.Stacks.Stack_Type);

   type Attack_Record_Array is array (Positive range <>) of Attack_Record;

   function Attack_Round
     (Battle : in out Battle_Record;
      Weapon : Carthage.Units.Weapon_Category)
      return Attack_Record_Array;

   procedure Scan_Battles
     (Process : not null access
        procedure (Battle : in out Battle_Record));

private

   type Attack_Record is
      record
         Attacker    : Carthage.Assets.Asset_Type;
         Defender    : Carthage.Assets.Asset_Type;
         Weapon      : Carthage.Units.Weapon_Category;
         Accuracy    : Natural;
         Agility     : Natural;
         Strength    : Natural;
         Armour      : Natural;
         Hit         : Boolean;
         Hit_Roll    : Positive;
         Damage_Roll : Positive;
         Damage      : Natural;
      end record;

   function Attacker
     (Attack : Attack_Record)
      return Carthage.Assets.Asset_Type
   is (Attack.Attacker);

   function Defender
     (Attack : Attack_Record)
      return Carthage.Assets.Asset_Type
   is (Attack.Defender);

   function Weapon
     (Attack : Attack_Record)
      return Carthage.Units.Weapon_Category
   is (Attack.Weapon);

   function Attack_Accuracy
     (Attack : Attack_Record)
      return Natural
   is (Attack.Accuracy);

   function Defense_Agility
     (Attack : Attack_Record)
      return Natural
   is (Attack.Agility);

   function Attack_Strength
     (Attack : Attack_Record)
      return Natural
   is (Attack.Strength);

   function Defense_Strength
     (Attack : Attack_Record)
      return Natural
   is (Attack.Armour);

   function Hit_Roll
     (Attack : Attack_Record)
      return Positive
   is (Attack.Hit_Roll);

   function Hit
     (Attack : Attack_Record)
      return Boolean
   is (Attack.Hit);

   function Damage_Roll
     (Attack : Attack_Record)
      return Positive
   is (Attack.Damage_Roll);

   function Damage
     (Attack : Attack_Record)
      return Natural
   is (Attack.Damage);

   package Asset_Vectors is
     new Ada.Containers.Vectors
       (Positive, Carthage.Assets.Asset_Type, Carthage.Assets."=");

   type Battle_Record is
      record
         Attacker  : Carthage.Houses.House_Type;
         Defender  : Carthage.Houses.House_Type;
         Attackers : Asset_Vectors.Vector;
         Defenders : Asset_Vectors.Vector;
      end record;

   function Attacker
     (Battle : Battle_Record)
      return Carthage.Houses.House_Type
   is (Battle.Attacker);

   function Defender
     (Battle : Battle_Record)
      return Carthage.Houses.House_Type
   is (Battle.Defender);

end Carthage.Combat;
