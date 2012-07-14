package body Win32 is

   function Cat (Left, Right : String) return String is
      Nul : constant Character := Character'First;
   begin
      if Left (Left'Last) = Nul then
         if Right (Right'Last) = Nul then
            return Left (Left'First .. Left'Last - 1) & Right;
         else
            return Left (Left'First .. Left'Last - 1) & Right & Nul;
         end if;
      else
         if Right (Right'Last) = Nul then
            return Left & Right;
         else
            return Left & Right & Nul;
         end if;
      end if;
   end Cat;

   function Cat (Left, Right : Wide_String) return Wide_String is
      Nul : constant Wide_Character := Wide_Character'First;
   begin
      if Left (Left'Last) = Nul then
         if Right (Right'Last) = Nul then
            return Left (Left'First .. Left'Last - 1) & Right;
         else
            return Left (Left'First .. Left'Last - 1) & Right & Nul;
         end if;
      else
         if Right (Right'Last) = Nul then
            return Left & Right;
         else
            return Left & Right & Nul;
         end if;
      end if;
   end Cat;

   function Cat (Left, Right : CHAR_Array) return CHAR_Array is
      Nul : constant CHAR := CHAR'First;
      use type Win32.CHAR;
   begin
      if Left (Left'Last) = Nul then
         if Right (Right'Last) = Nul then
            return Left (Left'First .. Left'Last - 1) & Right;
         else
            return Left (Left'First .. Left'Last - 1) & Right & Nul;
         end if;
      else
         if Right (Right'Last) = Nul then
            return Left & Right;
         else
            return Left & Right & Nul;
         end if;
      end if;
   end Cat;

   function Cat (Left, Right : WCHAR_Array) return WCHAR_Array is
      Nul : constant WCHAR := WCHAR'First;
      use type Win32.WCHAR;
   begin
      if Left (Left'Last) = Nul then
         if Right (Right'Last) = Nul then
            return Left (Left'First .. Left'Last - 1) & Right;
         else
            return Left (Left'First .. Left'Last - 1) & Right & Nul;
         end if;
      else
         if Right (Right'Last) = Nul then
            return Left & Right;
         else
            return Left & Right & Nul;
         end if;
      end if;
   end Cat;

   function Addr (S : String) return PSTR is
      function To_PSTR is new
        Ada.Unchecked_Conversion (System.Address, PSTR);
   begin
      return To_PSTR (S (S'First)'Address);
   end Addr;

   function Addr (S : String) return PCSTR is
   begin
      return To_PCSTR (S (S'First)'Address);
   end Addr;

   function Addr (S : Wide_String) return PWSTR is
   begin
      return To_PWSTR (S (S'First)'Address);
   end Addr;

   function Addr (S : Wide_String) return PCWSTR is
   begin
      return To_PCWSTR (S (S'First)'Address);
   end Addr;

   function Addr (S : CHAR_Array) return PSTR is
      function To_PSTR is new
        Ada.Unchecked_Conversion (System.Address, PSTR);
   begin
      return To_PSTR (S (S'First)'Address);
   end Addr;

   function Addr (S : CHAR_Array) return PCSTR is
   begin
      return To_PCSTR (S (S'First)'Address);
   end Addr;

   function Addr (S : WCHAR_Array) return PWSTR is
   begin
      return To_PWSTR (S (S'First)'Address);
   end Addr;

   function Addr (S : WCHAR_Array) return PCWSTR is
   begin
      return To_PCWSTR (S (S'First)'Address);
   end Addr;

   function To_Chars_Ptr (STR : PSTR) return Interfaces.C.Strings.chars_ptr is
      function UC1 is new
        Ada.Unchecked_Conversion (PSTR, Interfaces.C.Strings.chars_ptr);
   begin
      return UC1 (STR);
   end To_Chars_Ptr;

   function To_Chars_Ptr (STR : PCSTR) return Interfaces.C.Strings.chars_ptr is
      function UC2 is new
        Ada.Unchecked_Conversion (PCSTR, Interfaces.C.Strings.chars_ptr);
   begin
      return UC2 (STR);
   end To_Chars_Ptr;

   function To_PSTR (CP : Interfaces.C.Strings.chars_ptr) return PSTR is
      function UC3 is new
        Ada.Unchecked_Conversion (Interfaces.C.Strings.chars_ptr, PSTR);
   begin
      return UC3 (CP);
   end To_PSTR;

   function To_PCSTR (CP : Interfaces.C.Strings.chars_ptr) return PCSTR is
      function UC4 is new
        Ada.Unchecked_Conversion (Interfaces.C.Strings.chars_ptr, PCSTR);
   begin
      return UC4 (CP);
   end To_PCSTR;

   function To_C (S : CHAR_Array) return Interfaces.C.char_array is
      Res : Interfaces.C.char_array (
                                   Interfaces.C.size_t (S'First) ..
                                   Interfaces.C.size_t (S'Last));
   begin
      Res := Interfaces.C.char_array (S);
      return Res;
   end To_C;

   function To_Win (S : Interfaces.C.char_array) return CHAR_Array is
      Low  : constant Integer := Integer (S'First);
      High : constant Integer := Integer (S'Last);
      Res  : CHAR_Array (Low .. High);
   begin
      Res := CHAR_Array (S);
      return Res;
   end To_Win;

   function To_Win (S : Interfaces.C.wchar_array) return WCHAR_Array is
      Low  : constant Integer := Integer (S'First);
      High : constant Integer := Integer (S'Last);
      Res  : WCHAR_Array (Low .. High);
   begin
      Res := WCHAR_Array (S);
      return Res;
   end To_Win;

   ----------------------------------------------------------------------------
   --
   --  THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS FURNISHED "AS IS"
   --  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED,
   --  INCLUDING BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF
   --  MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.  The
   --  user assumes the entire risk as to the accuracy and the use of
   --  this file.
   --
   --  Copyright (c) Intermetrics, Inc. 1995
   --  Royalty-free, unlimited, worldwide, non-exclusive use, modification,
   --  reproduction and further distribution of this file is permitted.
   --
   ----------------------------------------------------------------------------

end Win32;

--  Log
--  05/04/1998 - remove Pragma Linker_Options ("-lwin32ada") - this option
--  is already set in the spec.


