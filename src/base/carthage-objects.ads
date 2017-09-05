private with Ada.Strings.Unbounded;

with Memor;

with Carthage.Named;

package Carthage.Objects is

   type Root_Carthage_Object is
     abstract new Memor.Root_Record_Type with private;

   type Root_Identifier_Object is
     abstract new Root_Carthage_Object
     and Memor.Identifier_Record_Type
   with private;

   procedure Create_With_Identity
     (Item     : in out Root_Identifier_Object'Class;
      Identity : String);

   overriding function Identifier
     (Item : Root_Identifier_Object)
      return String;

   function Log_Identifier
     (Item : Root_Identifier_Object)
      return String
   is (Root_Identifier_Object'Class (Item).Identifier);

   procedure Log
     (Object  : Root_Identifier_Object'Class;
      Message : String);

   type Root_Named_Object is
     abstract new Root_Identifier_Object
     and Carthage.Named.Named_Interface
   with private;

   overriding function Name (Item : Root_Named_Object) return String;
   procedure Set_Name (Item : in out Root_Named_Object'Class;
                       Name : in     String);

private

   type Root_Carthage_Object is abstract new Memor.Root_Record_Type with
     null record;

   type Root_Identifier_Object is
     abstract new Root_Carthage_Object
     and Memor.Identifier_Record_Type with
      record
         Object_Identifier : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Root_Named_Object is
     abstract new Root_Identifier_Object
     and Carthage.Named.Named_Interface
       with
      record
         Object_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Carthage.Objects;
