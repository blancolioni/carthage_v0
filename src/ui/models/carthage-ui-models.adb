with WL.String_Maps;

with Carthage.Updates;

with Carthage.UI.Models.Galaxy;

package body Carthage.UI.Models is

   package Model_Maps is new WL.String_Maps (Carthage_Model);

   Model_Map : Model_Maps.Map;

   ------------------
   -- After_Render --
   ------------------

   overriding procedure After_Render
     (Model    : in out Root_Carthage_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      pragma Unreferenced (Model, Renderer);
   begin
      Carthage.Updates.Render_Finished;
   end After_Render;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Model    : in out Root_Carthage_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      pragma Unreferenced (Model, Renderer);
   begin
      Carthage.Updates.Render_Started;
   end Before_Render;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (House : not null access constant Carthage.Houses.House_Record'Class;
      Key   : String)
      return Carthage_Model
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

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (House : not null access constant Carthage.Houses.House_Record'Class;
      Key   : String;
      Model : not null access Root_Carthage_Model'Class)
   is
   begin
      Model.House := Carthage.Houses.House_Type (House);
      Model_Map.Insert
        (House.Identifier & "--" & Key, Carthage_Model (Model));
   end Set_Model;

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
      return Carthage_Model
   is
      use type Lui.Models.Object_Model;
   begin
      if not Model_Map.Contains
        (House.Identifier)
      then
         Model_Map.Insert
           (House.Identifier,
            Carthage.UI.Models.Galaxy.Galaxy_Model (House));
      end if;

      return Model_Map.Element (House.Identifier);
   end Top_Model;

end Carthage.UI.Models;
