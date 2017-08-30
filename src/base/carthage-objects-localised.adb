with WL.Localisation;

package body Carthage.Objects.Localised is

   ----------
   -- Name --
   ----------

   function Name
     (Item : Root_Localised_Object'Class)
      return String
   is
   begin
      return WL.Localisation.Local_Text
        (Item.Local_Text_Key);
   end Name;

   ----------
   -- Name --
   ----------

   function Name
     (Item     : Root_Localised_Object'Class;
      Modifier : String)
      return String
   is
      Key : constant String := Item.Identifier & "-" & Modifier;
   begin
      if WL.Localisation.Has_Local_Text (Key) then
         return WL.Localisation.Local_Text (Key);
      else
         return Item.Name;
      end if;
   end Name;

end Carthage.Objects.Localised;
