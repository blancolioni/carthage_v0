private package Carthage.UI.Models.Galaxy is

   function Galaxy_Model
     (House : not null access constant Carthage.Houses.House_Class)
     return Lui.Models.Object_Model;

end Carthage.UI.Models.Galaxy;
