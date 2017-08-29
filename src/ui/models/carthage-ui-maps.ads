private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Tropos;

with Carthage.Colours;
with Carthage.Houses;
with Carthage.Planets;

package Carthage.UI.Maps is

   type Layer_Element_Type is
     (Background_Hex_Tile, Hex_Tile, Icon);

   type Tile_Layers is tagged limited private;

   procedure Get_Tile_Layers
     (Planet   : Carthage.Planets.Planet_Type;
      House    : Carthage.Houses.House_Type;
      Position : Tile_Position;
      Layers   : in out Tile_Layers'Class);

   procedure Scan_Layers
     (Layers  : Tile_Layers'Class;
      Process : not null access
        procedure (Element : Layer_Element_Type;
                   Resource_Name : String;
                   Color : Carthage.Colours.Colour_Type));

   procedure Configure_Tile_Resources
     (Config : Tropos.Configuration);

private

   type Layer_Element (Length : Natural) is
      record
         Layer_Element : Layer_Element_Type;
         Resource_Name : String (1 .. Length);
         Color         : Carthage.Colours.Colour_Type;
      end record;

   package Resource_Name_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Layer_Element);

   type Tile_Layers is tagged limited
      record
         List : Resource_Name_Lists.List;
      end record;

end Carthage.UI.Maps;
