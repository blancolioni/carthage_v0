with Carthage.Assets;

package Carthage.UI.Models.Stacks is

   function Asset_Resource
     (Asset : Carthage.Assets.Asset_Type)
      return String;

private

   function Asset_Resource
     (Asset : Carthage.Assets.Asset_Type)
      return String
   is ("unit"
       & Integer'Image (-(Asset.Unit.Index)));

end Carthage.UI.Models.Stacks;
