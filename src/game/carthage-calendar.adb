package body Carthage.Calendar is

   Local_Today : Date := 0;

   -------------
   -- Date_Of --
   -------------

   function Date_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number)
      return Date
   is
   begin
      return Date (Day - 1 + 30 * (Month - 1 + 12 * (Year - Start_Year)));
   end Date_Of;

   ---------
   -- Day --
   ---------

   function Day (Day : Date) return Day_Number is
   begin
      return Natural (Day) mod 30 + 1;
   end Day;

   --------------------
   -- Day_Identifier --
   --------------------

   function Day_Identifier
     (D : Date)
      return String
   is
      Year   : Year_Number;
      Month  : Month_Number;
      Day    : Day_Number;
      Result : String := "0000-00-00";
      It     : Natural;
   begin
      Split (D, Year, Month, Day);
      It := Day + 100 * (Month + 100 * Year);
      for Ch of reverse Result loop
         if Ch = '0' then
            Ch := Character'Val (It mod 10 + 48);
            It := It / 10;
         end if;
      end loop;
      return Result;
   end Day_Identifier;

   -----------
   -- Month --
   -----------

   function Month (Day : Date) return Month_Number is
   begin
      return Natural (Day) / 30 mod 12 + 1;
   end Month;

   --------------
   -- Next_Day --
   --------------

   procedure Next_Day is
   begin
      Local_Today := Local_Today + 1;
   end Next_Day;

   --------------
   -- Set_Date --
   --------------

   procedure Set_Date
     (Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number)
   is
   begin
      Local_Today := Date_Of (Year, Month, Day);
   end Set_Date;

   -----------
   -- Split --
   -----------

   procedure Split
     (D       : Date;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number)
   is
   begin
      Year := Carthage.Calendar.Year (D);
      Month := Carthage.Calendar.Month (D);
      Day := Carthage.Calendar.Day (D);
      pragma Assert (Date_Of (Year, Month, Day) = D);
   end Split;

   -----------
   -- Today --
   -----------

   function Today return Date is
   begin
      return Local_Today;
   end Today;

   ----------
   -- Year --
   ----------

   function Year (Day : Date) return Year_Number is
   begin
      return Natural (Day) / 360 + Start_Year;
   end Year;

end Carthage.Calendar;
