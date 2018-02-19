with Ada.Text_IO;

with Carthage.Calendar;

package body Carthage.Logging is

   Logging_Enabled         : Boolean := False;
   Logging_Level           : Log_Level;
   Standard_Output_Logging : Boolean := False;
   Log_File : Ada.Text_IO.File_Type;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      Log (3, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Level   : Log_Level;
      Message : String)
   is
      Log_Message : constant String :=
                      Carthage.Calendar.Image
                        (Carthage.Calendar.Clock, True)
                      & ": " & Message;
   begin
      if Logging_Enabled and then Level <= Logging_Level then
         if Standard_Output_Logging then
            Ada.Text_IO.Put_Line (Log_Message);
         else
            Ada.Text_IO.Put_Line (Log_File, Log_Message);
         end if;
         if Level = 1 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, Log_Message);
         end if;
      end if;
   end Log;

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception
     (Message : String;
      E       : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Log_Exception (1, Message, E);
   end Log_Exception;

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception
     (Level   : Log_Level;
      Message : String;
      E       : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Log
        (Level => Level,
         Message =>
           Message & ": "
         & Ada.Exceptions.Exception_Name (E)
         & ": " & Ada.Exceptions.Exception_Message (E));
   end Log_Exception;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging
     (Path : String := "";
      Level : Log_Level := 4)
   is
   begin
      Logging_Enabled := True;
      Logging_Level   := Level;
      if Path = "" then
         Standard_Output_Logging := True;
      else
         begin
            Ada.Text_IO.Create
              (Log_File, Ada.Text_IO.Out_File, Path);
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "cannot open log file [" & Path & "] for writing: "
                  & Ada.Exceptions.Exception_Name (E)
                  & ": " & Ada.Exceptions.Exception_Message (E));
               Standard_Output_Logging := True;
         end;
      end if;
   end Start_Logging;

   ------------------
   -- Stop_Logging --
   ------------------

   procedure Stop_Logging is
   begin
      if Logging_Level > 1 then
         Log (Logging_Level, "end of log");
      end if;
      Logging_Enabled := False;

      if not Standard_Output_Logging then
         Ada.Text_IO.Close (Log_File);
      end if;

   end Stop_Logging;

end Carthage.Logging;
