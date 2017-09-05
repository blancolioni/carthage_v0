package Carthage.Calendar is

   Start_Year : constant := 4956;
   End_Year   : constant := 5001;

   type Date is private;

   subtype Year_Number  is Integer range Start_Year .. End_Year;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number   is Integer range 1 .. 30;

   function Today return Date;

   function Year    (Day : Date) return Year_Number;
   function Month   (Day : Date) return Month_Number;
   function Day     (Day : Date) return Day_Number;

   procedure Split
     (D       : Date;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number);

   function Date_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number)
      return Date;

   function Day_Identifier
     (D : Date)
      return String;

   procedure Set_Date
     (Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number);

   procedure Next_Day;

private

   type Date is range 0 .. (End_Year - Start_Year + 1) * 365;

end Carthage.Calendar;
