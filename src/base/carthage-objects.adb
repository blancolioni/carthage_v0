with Carthage.Logging;

package body Carthage.Objects is

   Update_List : Memor.Memor_Update_List;

   -----------------------
   -- Add_Object_Update --
   -----------------------

   procedure Add_Object_Update
     (Update : Memor.Object_Update_Interface'Class)
   is
   begin
      Memor.Add_Update (Update_List, Update);
   end Add_Object_Update;

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

   ----------------------------
   -- Execute_Object_Updates --
   ----------------------------

   procedure Execute_Object_Updates is
   begin
      Memor.Execute_Updates (Update_List);
   end Execute_Object_Updates;

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
        ("[" & Object.Log_Identifier & "]: " & Message);
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
