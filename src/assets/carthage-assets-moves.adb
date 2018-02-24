with Ada.Containers.Indefinite_Holders;

with WL.Random;

with Carthage.Calendar;
with Carthage.Updates;

with Carthage.Galaxy;
with Carthage.Stacks;

package body Carthage.Assets.Moves is

   package Jump_Route_Holders is
     new Ada.Containers.Indefinite_Holders
       (Carthage.Planets.Array_Of_Planets,
        Carthage.Planets."=");

   type Jump_Update is
     new Carthage.Updates.Update_Interface with
      record
         Asset : Asset_Type;
         Path  : Jump_Route_Holders.Holder;
      end record;

   overriding procedure Activate
     (Upd : Jump_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Upd : Jump_Update)
   is
      Route : constant Carthage.Planets.Array_Of_Planets :=
                Upd.Path.Element;
      Planet : constant Carthage.Planets.Planet_Type :=
                 Route (Route'First);
      Stack  : constant Carthage.Stacks.Stack_Type :=
                 Planet.Orbital_Stack (Upd.Asset.Owner);
   begin
      Upd.Asset.Log ("jumping to " & Planet.Name);
      Upd.Asset.Move_To (Stack);
      Planet.Update.Set_Seen_By (Stack.Owner);
      if Route'Length > 1 then
         Start_Jump (Upd.Asset, Route (Route'First), Route (Route'First + 1));
      end if;
   end Activate;

   ----------------
   -- Start_Jump --
   ----------------

   procedure Start_Jump
     (Asset       : Asset_Type;
      Start       : Carthage.Planets.Planet_Type;
      Destination : Carthage.Planets.Planet_Type)
   is
      Delay_Duration : constant Duration :=
                         Carthage.Calendar.Hours (12)
                         + Duration (WL.Random.Random_Number
                                     (1, 12 * 3600));
      Route          : constant Carthage.Planets.Array_Of_Planets :=
                         Carthage.Galaxy.Jump_Route (Start, Destination);
      Update         : constant Jump_Update :=
                         Jump_Update'
                           (Asset => Asset,
                            Path  =>
                              Jump_Route_Holders.To_Holder (Route));
   begin
      Asset.Update.Jumping := True;
      Carthage.Updates.Queue (Update, Delay_Duration);
   end Start_Jump;

   procedure Start_Landing
     (Asset : Asset_Type;
      Tile  : Carthage.Tiles.Tile_Type)
   is null;

   procedure Start_Launch
     (Asset : Asset_Type)
   is null;

end Carthage.Assets.Moves;
