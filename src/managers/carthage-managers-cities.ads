with Carthage.Cities;
with Carthage.Houses;

package Carthage.Managers.Cities is

   type City_Trade_Group is private;

   function New_Trade_Group return City_Trade_Group;

   function Create_City_Manager
     (House  : Carthage.Houses.House_Type;
      Group  : City_Trade_Group;
      City   : not null access constant Carthage.Cities.City_Record'Class)
      return Manager_Type;

private

   type City_Trade_Group_Record;

   type City_Trade_Group is access City_Trade_Group_Record;

end Carthage.Managers.Cities;
