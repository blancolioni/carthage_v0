package Carthage.Paths is

   Config_Path : constant String :=
     "E:\home\fraser\Documents\kiln\carthage\config";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Carthage.Paths;
