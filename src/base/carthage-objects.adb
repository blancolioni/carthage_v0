with Carthage.Logging;

package body Carthage.Objects is

   --------------------------
   -- Create_With_Identity --
   --------------------------

   procedure Create_With_Identity
     (Item     : in out Root_Identifier_Object'Class;
      Identity : String)
   is
   begin
      Item.Object_Identifier :=
        Ada.Strings.Unbounded.To_Unbounded_String (Identity);
   end Create_With_Identity;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Item : Root_Identifier_Object)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Object_Identifier);
   end Identifier;

   ---------
   -- Log --
   ---------

   procedure Log
     (Object  : Root_Identifier_Object'Class;
      Message : String)
   is
   begin
      Carthage.Logging.Log
        ("[" & Object.Identifier & "]: " & Message);
   end Log;

   ----------
   -- Name --
   ----------

   overriding function Name (Item : Root_Named_Object) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Object_Name);
   end Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Item : in out Root_Named_Object'Class;
      Name : in     String)
   is
   begin
      Item.Object_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Carthage.Objects;
