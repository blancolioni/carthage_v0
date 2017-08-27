package Carthage.Objects.Localised is

   type Root_Localised_Object is
     abstract new Root_Identifier_Object
   with private;

   function Name
     (Item : Root_Localised_Object'Class)
      return String;

   function Name
     (Item     : Root_Localised_Object'Class;
      Modifier : String)
      return String;

   function Adjective
     (Item : Root_Localised_Object'Class)
      return String;

   function Plural
     (Item : Root_Localised_Object'Class)
      return String;

   function Full_Name
     (Item : Root_Localised_Object'Class)
      return String;

private

   type Root_Localised_Object is
     abstract new Root_Identifier_Object
   with null record;

   function Adjective
     (Item : Root_Localised_Object'Class)
      return String
   is (Item.Name ("adjective"));

   function Plural
     (Item : Root_Localised_Object'Class)
      return String
   is (Item.Name ("plural"));

   function Full_Name
     (Item : Root_Localised_Object'Class)
      return String
   is (Item.Name ("full"));

end Carthage.Objects.Localised;
