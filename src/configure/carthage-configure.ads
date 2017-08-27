package Carthage.Configure is

   procedure Load_Configuration;

   procedure Load_Scenario (Name : String);

   function Fading_Suns_Bin_File
     (Name : String)
      return String;

   function Fading_Suns_Data_File
     (Name : String)
      return String;

   function Fading_Suns_FLC_File
     (Name : String)
      return String;

   function Fading_Suns_Rand_File
     (Name : String)
      return String;

end Carthage.Configure;
