with Ada.Exceptions;

package Carthage.Logging is

   type Log_Level is range 1 .. 4;

   procedure Start_Logging (Path : String := "";
                            Level : Log_Level := 4);
   procedure Stop_Logging;

   procedure Log (Message : String);
   procedure Log (Level   : Log_Level;
                  Message : String);

   procedure Log_Exception
     (Message : String;
      E       : Ada.Exceptions.Exception_Occurrence);

   procedure Log_Exception
     (Level   : Log_Level;
      Message : String;
      E       : Ada.Exceptions.Exception_Occurrence);

end Carthage.Logging;
