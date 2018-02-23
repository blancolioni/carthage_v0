with Carthage.Planets;
with Carthage.Tiles;

package Carthage.Assets.Moves is

   procedure Start_Jump
     (Asset       : Asset_Type;
      Start       : Carthage.Planets.Planet_Type;
      Destination : Carthage.Planets.Planet_Type);

   procedure Start_Launch
     (Asset : Asset_Type);

   procedure Start_Landing
     (Asset : Asset_Type;
      Tile  : Carthage.Tiles.Tile_Type);

end Carthage.Assets.Moves;
