with Tropos;

package Carthage.Colours is

   type Colour_Element is new Float range 0.0 .. 1.0;

   type Colour_Type is
      record
         Red, Green, Blue, Alpha : Colour_Element;
      end record;

   function Configure
     (Config : Tropos.Configuration)
      return Colour_Type;

end Carthage.Colours;
