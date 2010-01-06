--  (c) Martin M. Dowie, 2003-2004

pragma License (Modified_GPL);

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;

with GNAT.Calendar;

with Interfaces.C.Strings; use Interfaces.C.Strings;

with Win32.crt.Stat;
with Win32.crt.Time;
with Win32.crt.Utime;

package body Ada_Directories_Extensions is


   type utimbuf_ptr is access all Win32.crt.Utime.utimbuf;
   pragma Convention (C, utimbuf_ptr);

   Null_utimbuf_ptr : constant utimbuf_ptr := null;

   --------------
   -- To_PCSTR --
   --------------

   function To_PCSTR is
      new Ada.Unchecked_Conversion (Interfaces.C.Strings.chars_ptr,
                                    Win32.PCSTR);

   -----------
   -- To_tm --
   -----------

   function To_tm (From : Ada.Calendar.Time)
      return Win32.crt.Time.tm;

   -----------
   -- utime --
   -----------

   --  Can't use Win32.crt.Utime binding as it is wrong. It uses
   --  an access parameter which means we can't pass 'NULL' which
   --  is actually a valid value to pass and the one required to
   --  provide this 'touch' facility.
   --
   function utime (filename : Interfaces.C.Strings.chars_ptr;
                   utimbuf : utimbuf_ptr)
      return Interfaces.C.int;
   pragma Import (C, utime, "_utime");

   -----------------------
   -- Set_Access_Time --
   -----------------------

   procedure Set_Access_Time (Directory_Entry : in Directory_Entry_Type;
                              To                : in Ada.Calendar.Time) is
   begin
      Set_Access_Time (Full_Name (Directory_Entry), To);
   end Set_Access_Time;

   -----------------------
   -- Set_Access_Time --
   -----------------------

   procedure Set_Access_Time (Name : in String;
                              To   : in Ada.Calendar.Time) is
      use type Interfaces.C.int;
      File_Stat : aliased Win32.crt.Stat.struct_stat;
      C_Name : constant Win32.PCSTR :=
         To_PCSTR (New_String (Full_Name (Name)));
      Ok : constant Interfaces.C.int :=
         Win32.crt.Stat.stat (C_Name, File_Stat'Access);
   begin
      if Ok /= 0 then
         -- Validate (Name);
         Raise_Exception (Use_Error'Identity,
                          "Unknown fault in setting access time");
      end if;
      declare
         Tm : aliased Win32.crt.Time.tm := To_tm (To);
         Buffer : aliased Win32.crt.Utime.utimbuf :=
            (acTime => Win32.crt.Time.mktime (Tm'Access),
             modTime => File_Stat.st_mtime);
         Ok : constant Interfaces.C.int :=
            utime (New_String (Full_Name (Name)),
                   Buffer'Unchecked_Access);
      begin
         if Ok /= 0 then
            Raise_Exception (Program_Error'Identity,
                             "Could not change time - check access rights [" &
                                Simple_Name (Name) & "]");
         end if;
      end;
   end Set_Access_Time;

   ---------------------------
   -- Set_Modification_Time --
   ---------------------------

   procedure Set_Modification_Time (Directory_Entry : in Directory_Entry_Type;
                                    To              : in Ada.Calendar.Time) is
   begin
      Set_Modification_Time (Full_Name (Directory_Entry), To);
   end Set_Modification_Time;

   ---------------------------
   -- Set_Modification_Time --
   ---------------------------

   procedure Set_Modification_Time (Name : in String;
                                    To   : in Ada.Calendar.Time) is
      use type Interfaces.C.int;
      File_Stat : aliased Win32.crt.Stat.struct_stat;
      C_Name : constant Win32.PCSTR :=
         To_PCSTR (New_String (Full_Name (Name)));
      Ok : constant Interfaces.C.int :=
         Win32.crt.Stat.stat (C_Name, File_Stat'Access);
   begin
      if Ok /= 0 then
         -- Validate (Name);
         Raise_Exception (Use_Error'Identity,
                          "Unknown fault in setting modification time");
      end if;
      declare
         Tm : aliased Win32.crt.Time.tm := To_tm (To);
         Buffer : aliased Win32.crt.Utime.utimbuf :=
            (acTime => File_Stat.st_atime,
             modTime => Win32.crt.Time.mktime (Tm'Access));
         Ok : constant Interfaces.C.int :=
            utime (New_String (Full_Name (Name)),
                   Buffer'Unchecked_Access);
      begin
         if Ok /= 0 then
            Raise_Exception (Program_Error'Identity,
                             "Could not change modification time - check " &
                                "access rights [" & Simple_Name (Name) & "]");
         end if;
      end;
   end Set_Modification_Time;

   ---------------
   -- Set_Times --
   ---------------

   procedure Set_Times (Directory_Entry : in Directory_Entry_Type;
                        Access_Time : in Ada.Calendar.Time;
                        Modification_Time : in Ada.Calendar.Time) is
   begin
      Set_Times (Full_Name (Directory_Entry), Access_Time, Modification_Time);
   end Set_Times;

   ---------------
   -- Set_Times --
   ---------------

   procedure Set_Times (Name : in String;
                        Access_Time : in Ada.Calendar.Time;
                        Modification_Time : in Ada.Calendar.Time) is
      use type Interfaces.C.int;
      File_Stat : aliased Win32.crt.Stat.struct_stat;
      C_Name : constant Win32.PCSTR :=
         To_PCSTR (New_String (Full_Name (Name)));
      Ok : constant Interfaces.C.int :=
         Win32.crt.Stat.stat (C_Name, File_Stat'Access);
   begin
      if Ok /= 0 then
         -- Validate (Name);
         Raise_Exception (Use_Error'Identity,
                          "Unknown fault in setting times");
      end if;
      declare
         Atm : aliased Win32.crt.Time.tm := To_tm (Access_Time);
         Mtm : aliased Win32.crt.Time.tm := To_tm (Modification_Time);
         Buffer : aliased Win32.crt.Utime.utimbuf :=
            (acTime => Win32.crt.Time.mktime (Atm'Access),
             modTime => Win32.crt.Time.mktime (Mtm'Access));
         Ok : constant Interfaces.C.int :=
            utime (New_String (Full_Name (Name)),
                   Buffer'Unchecked_Access);
      begin
         if Ok /= 0 then
            Raise_Exception (Program_Error'Identity,
                             "Could not change times - check " &
                                "access rights [" & Simple_Name (Name) & "]");
         end if;
      end;
   end Set_Times;

   -----------------------------
   -- Touch_Modification_Time --
   -----------------------------

   procedure Touch_Modification_Time
      (Directory_Entry : in Directory_Entry_Type) is
   begin
      Touch_Modification_Time (Full_Name (Directory_Entry));
   end Touch_Modification_Time;

   -----------------------------
   -- Touch_Modification_Time --
   -----------------------------

   procedure Touch_Modification_Time (Name : in String) is

      use type Interfaces.C.int;

      Ok : constant Interfaces.C.int := utime (New_String (Full_Name (Name)),
                                               Null_utimbuf_ptr);
   begin
      if Ok /= 0 then
         -- Validate (Name);
         Raise_Exception (Program_Error'Identity,
                          "Could not change time - check access rights [" &
                             Simple_Name (Name) & "]");
      end if;
   end Touch_Modification_Time;

   -----------
   -- To_tm --
   -----------

   function To_tm (From : Ada.Calendar.Time)
      return Win32.crt.Time.tm is
      use type Win32.INT;
      Year       : Ada.Calendar.Year_Number;
      Month      : Ada.Calendar.Month_Number;
      Day        : Ada.Calendar.Day_Number;
      Hour       : GNAT.Calendar.Hour_Number;
      Minute     : GNAT.Calendar.Minute_Number;
      Second     : GNAT.Calendar.Second_Number;
      Sub_Second : GNAT.Calendar.Second_Duration;
   begin
      GNAT.Calendar.Split
         (From, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return (tm_sec   => Win32.INT (Second),
              tm_min   => Win32.INT (Minute),
              tm_hour  => Win32.INT (Hour),
              tm_mday  => Win32.INT (Day),
              tm_mon   => Win32.INT (Month) - 1,
              tm_year  => Win32.INT (Year) - 1900,
              tm_wday  => Win32.INT (GNAT.Calendar.Day_Name'Pos
                                        (GNAT.Calendar.Day_Of_Week (From))),
              tm_yday  => Win32.INT (GNAT.Calendar.Day_In_Year (From)),
              tm_isdst => 0);  -- Don't care for this package
   end To_tm;

   --------------
   -- Validate --
   --------------

   procedure Validate (Name : in Directory_Entry_Type) is
   begin
      Validate (Full_Name (Name));
   end Validate;

   --------------
   -- Validate --
   --------------

   procedure Validate (Name : in String) is
   begin
      --  if not Port.Is_Valid_Filename (Name) then
      --     Raise_Exception (Name_Error'Identity,
      --                      "Invalid name [" & Name & "]");
      --  end if;
      if not Exists (Name) then
         Raise_Exception (Use_Error'Identity,
                          "Does not exist [" & Name & "]");
      end if;
   end Validate;

end Ada_Directories_Extensions;
