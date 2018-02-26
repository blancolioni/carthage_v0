with WL.String_Maps;

with Carthage.UI.Models.Top;

package body Carthage.UI.Models is

   package Model_Maps is
     new WL.String_Maps (Lui.Models.Object_Model, Lui.Models."=");

   Model_Map : Model_Maps.Map;

   ------------------
   -- After_Render --
   ------------------

--     overriding procedure After_Render
--       (Model    : in out Root_Carthage_Model;
--        Renderer : in out Lui.Rendering.Root_Renderer'Class)
--     is null;

   -------------------
   -- Before_Render --
   -------------------

--     overriding procedure Before_Render
--       (Model    : in out Root_Carthage_Model;
--        Renderer : in out Lui.Rendering.Root_Renderer'Class)
--     is null;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (House : not null access constant Carthage.Houses.House_Record'Class;
      Key   : String)
      return Lui.Models.Object_Model
   is
   begin
      return Model_Map.Element (House.Identifier & "--" & Key);
   end Get_Model;

   ----------------
   -- Have_Model --
   ----------------

   function Have_Model
     (House : not null access constant Carthage.Houses.House_Record'Class;
      Key   : String)
      return Boolean
   is
   begin
      return Model_Map.Contains (House.Identifier & "--" & Key);
   end Have_Model;

   ----------------
   -- Save_Model --
   ----------------

   procedure Save_Model
     (Model    : not null access Lui.Models.Root_Object_Model'Class;
      Class_Id : String)
   is
      use Carthage.UI.Models.Top;
      M : constant Carthage_Model := Carthage_Model (Model);
   begin
      Model_Map.Insert
        (M.House.Identifier & "--" & Class_Id,
         Model);
   end Save_Model;

   -------------------
   -- To_Lui_Colour --
   -------------------

   function To_Lui_Colour
     (Colour : Carthage.Colours.Colour_Type)
      return Lui.Colours.Colour_Type
   is
      use Carthage.Colours;
      use Lui.Colours;
   begin
      return Apply_Alpha
        (To_Colour
           (Red   => Colour_Byte (Colour.Red * 255.0),
            Green => Colour_Byte (Colour.Green * 255.0),
            Blue  => Colour_Byte (Colour.Blue * 255.0)),
         Lui.Unit_Real (Colour.Alpha));
   end To_Lui_Colour;

   ---------------
   -- Top_Model --
   ---------------

   function Top_Model
     (House : not null access constant Carthage.Houses.House_Class)
      return Lui.Models.Object_Model
   is
      use type Lui.Models.Object_Model;
   begin
      if not Model_Map.Contains
        (House.Identifier)
      then
         Model_Map.Insert
           (House.Identifier,
            Carthage.UI.Models.Top.Top_Model (House));
      end if;

      return Model_Map.Element (House.Identifier);
   end Top_Model;

end Carthage.UI.Models;
