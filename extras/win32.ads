--  **************************************************************
--  Parent package for other Win32 interfaces.
--  The packages are organized into one library package with child
--  packages to avoid library level namespace pollution.
--  Some types are moved up here from winnt, winbase, and windefs,
--  to remove circular dependencies between those packages.
--  Also some common types, e.g. CHAR_Array, are put here so they
--  will not be replicated in many other packages.
--  **************************************************************

with Ada.Unchecked_Conversion;
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

package Win32 is

   pragma Linker_Options ("-lwin32ada");
   pragma Linker_Options ("-luser32");
   pragma Linker_Options ("-lgdi32");
   pragma Linker_Options ("-lcomdlg32");

   --  *****************************************************
   --  Built-in C types, their Win32 names,
   --  pointers to "all" and "constant" values of the types,
   --  and arrays of the types.
   --  *****************************************************

   --  C pointer to "void", a pointer to anything that must
   --  be converted to another pointer to be useful
   type    VOID    is null record;
   subtype PVOID   is System.Address;                      --  winnt.h
   subtype LPVOID  is PVOID;                               --  windef.h
   subtype PCVOID  is PVOID;
   subtype LPCVOID is PCVOID;                              --  windef.h
   type    PVOID_Array is array (Natural range <>) of aliased PVOID;
   --  rpcproxy.h

   --  8 bit signed chars
   subtype CHAR       is Interfaces.C.char;                --  winnt.h
   type    PCHAR      is access all CHAR;                  --  winnt.h
   subtype LPCH       is PCHAR;                            --  winnt.h
   subtype PCH        is PCHAR;                            --  winnt.h
   subtype NPSTR      is PCHAR;                            --  winnt.h
   subtype LPSTR      is PCHAR;                            --  winnt.h
   subtype PSTR       is PCHAR;                            --  winnt.h
   subtype PSZ        is PCHAR;                            --  windef.h
   type    PCCH       is access constant CHAR;             --  winnt.h
   subtype PCCHAR     is PCCH;                             --  rpcproxy.h
   subtype LPCCH      is PCCH;                             --  winnt.h
   subtype LPCSTR     is PCCH;                             --  winnt.h
   subtype PCSTR      is PCCH;                             --  winnt.h
   type    CHAR_Array is array (Natural range <>) of aliased CHAR;
   Nul     : constant CHAR := CHAR'First;
   type    TCHAR      is new CHAR;                         --  tchar.h :409

   function To_PCSTR is new Ada.Unchecked_Conversion (System.Address, PCSTR);

   --  8 bit unsigned chars
   subtype UCHAR       is Interfaces.C.unsigned_char;      --  windef.h
   type    BYTE        is new Interfaces.C.unsigned_char;  --  windef.h
   type    PBYTE       is access all BYTE;                 --  windef.h
   subtype LPBYTE      is PBYTE;                           --  windef.h
   type    PCBYTE      is access constant BYTE;
   type    PCUCHAR     is access constant UCHAR;           --  rpcndr.h
   type    PUCHAR      is access all UCHAR;                --  windef.h
   type    BYTE_Array  is array (Natural range <>) of aliased BYTE;
   type    UCHAR_Array is array (Natural range <>) of aliased UCHAR;

   function To_PUCHAR is new Ada.Unchecked_Conversion (PVOID, PUCHAR);
   function To_PBYTE is new Ada.Unchecked_Conversion (PVOID, PBYTE);

   --  16 bit wide chars
   subtype Wchar_T     is Interfaces.C.wchar_t;            --  ctype.h
   subtype WCHAR       is Wchar_T;                         --  winnt.h
   type    PWCH        is access all WCHAR;                --  winnt.h
   subtype LPWCH       is PWCH;                            --  winnt.h
   subtype PWCHAR      is PWCH;                            --  winnt.h
   subtype NWPSTR      is PWCH;                            --  winnt.h
   subtype LPWSTR      is PWCH;                            --  winnt.h
   subtype PWSTR       is PWCH;                            --  winnt.h
   type    PCWCH       is access constant WCHAR;           --  winnt.h
   subtype LPCWCH      is PCWCH;                           --  winnt.h
   subtype PCWSTR      is PCWCH;                           --  winnt.h
   subtype LPCWSTR     is PCWCH;                           --  winnt.h
   type    WCHAR_Array is array (Natural range <>) of aliased WCHAR;
   Wide_Nul : constant WCHAR := WCHAR'First;

   function To_LPCWSTR is new Ada.Unchecked_Conversion (PCBYTE, LPCWSTR);
   function To_PWSTR is new Ada.Unchecked_Conversion (System.Address, PWSTR);
   function To_PCWSTR is new Ada.Unchecked_Conversion (System.Address, PCWSTR);

   --  16 bit signed integers
   subtype SHORT       is Interfaces.C.short;              --  winnt.h
   type    PSHORT      is access all SHORT;                --  windef.h
   type    PCSHORT     is access constant SHORT;
   type    SHORT_Array is array (Natural range <>) of aliased SHORT;

   --  16 bit unsigned integers
   subtype USHORT     is Interfaces.C.unsigned_short;      --  windef.h
   subtype WORD       is USHORT;                           --  windef.h
   type    PUSHORT    is access all USHORT;                --  windef.h
   type    PWORD      is new PUSHORT;                      --  windef.h
   type    PCWORD     is access constant WORD;
   subtype LPWORD     is PWORD;                            --  windef.h
   type    WORD_Array is array (Natural range <>) of aliased WORD;

   --  "int" types, signed, 32 bits on Intel
   subtype INT       is Interfaces.C.int;                  --  windef.h
   type    BOOL      is new INT;                           --  windef.h
   type    PBOOL     is access all BOOL;                   --  windef.h
   subtype LPBOOL    is PBOOL;                             --  windef.h
   type    PINT      is access all INT;                    --  windef.h
   subtype LPINT     is PINT;                              --  windef.h
   type    PCINT     is access constant INT;
   type    INT_Array is array (Natural range <>) of aliased INT;

   function To_PINT is new Ada.Unchecked_Conversion (System.Address, PINT);
   function To_PBOOL is new Ada.Unchecked_Conversion(PVOID, PBOOL);

   --  "unsigned" or "unsigned int" types, unsigned, 32 bits on Intel
   subtype UINT       is Interfaces.C.unsigned;            --  windef.h
   subtype WPARAM     is UINT;                             --  windef.h
   type    PUINT      is access all UINT;                  --  windef.h
   type    PCUINT     is access constant UINT;
   type    UINT_Array is array (Natural range <>) of aliased UINT;
   subtype Size_T     is Interfaces.C.size_t;

   --  "long" 32 bit signed integers
   subtype LONG       is Interfaces.C.long;                --  winnt.h
   subtype LPARAM     is LONG;                             --  windef.h
   subtype LRESULT    is LONG;                             --  windef.h
   type    PLONG      is access all LONG;
   subtype LPLONG     is PLONG;                            --  windef.h
   type    PCLONG     is access constant LONG;
   type    LONG_Array is array (Natural range <>) of aliased LONG;

   --  "unsigned long" 32 bit unsigned integers
   subtype ULONG       is Interfaces.C.unsigned_long;      --  windef.h
   subtype DWORD       is ULONG;                           --  windef.h
   type    PULONG      is access all ULONG;                --  windef.h
   subtype PDWORD      is PULONG;                          --  windef.h
   subtype LPDWORD     is PDWORD;                          --  windef.h
   type    PCDWORD     is access constant DWORD;
   type    ULONG_Array is array (Natural range <>) of aliased ULONG;
   subtype DWORD_Array is ULONG_Array;

   function To_PULONG is new Ada.Unchecked_Conversion (System.Address, PULONG);
   function To_LPTSTR is new Ada.Unchecked_Conversion (DWORD, PSTR);
   function To_LPWSTR is new Ada.Unchecked_Conversion (DWORD, LPWSTR);

   --  builtin C "float", 32 bits on Intel
   subtype FLOAT  is Interfaces.C.C_float;                 --  windef.h
   type    PFLOAT is access all FLOAT;                     --  windef.h

   --  builtin C "double", also 64 bits on Intel
   subtype DOUBLE  is Interfaces.C.double;
   type    PDOUBLE is access all DOUBLE;

   --  64 bit signed integer
   type LONGLONG is private;
   type PLONGLONG is access all LONGLONG;                  --  winnt.h
   type PCLONGLONG is access constant LONGLONG;

   --  64 bit unsigned integer
   type DWORDLONG is private;
   type PDWORDLONG is access all DWORDLONG;                --  winnt.h
   type PCDWORDLONG is access constant DWORDLONG;

   --  ************************************************************
   --  True and False constants.
   --  Warning : when you get a value back from Windows, comparing
   --  Value /= FALSE is safe, comparing Value = TRUE is not safe,
   --  since Windows sometimes returns any non-zero value for TRUE.
   --  ************************************************************

   FALSE : constant := 0;                                  --  windef.h
   TRUE  : constant := 1;                                  --  windef.h

   --  ********************************************************
   --  so Interfaces.C and Windows strings can be used together
   --  ********************************************************

   function To_Chars_Ptr (STR : PSTR) return Interfaces.C.Strings.chars_ptr;

   function To_Chars_Ptr (STR : PCSTR) return Interfaces.C.Strings.chars_ptr;

   function To_PSTR (CP : Interfaces.C.Strings.chars_ptr) return PSTR;

   function To_PCSTR (CP : Interfaces.C.Strings.chars_ptr) return PCSTR;

   function To_C (S : CHAR_Array) return Interfaces.C.char_array;

   function To_Win (S : Interfaces.C.char_array) return CHAR_Array;

   function To_Win (S : Interfaces.C.wchar_array) return WCHAR_Array;

   --  ********************************************************************
   --  These could be useful with a Standard.String that is NUL-terminated.
   --  They return the address of the first element.
   --  See also the conversion functions in Interfaces.C.
   --  ********************************************************************

   function Addr (S : String) return PSTR;

   function Addr (S : String) return PCSTR;

   function Addr (S : Wide_String) return PWSTR;

   function Addr (S : Wide_String) return PCWSTR;

   function Addr (S : CHAR_Array) return PSTR;

   function Addr (S : CHAR_Array) return PCSTR;

   function Addr (S : WCHAR_Array) return PWSTR;

   function Addr (S : WCHAR_Array) return PCWSTR;

   --  **************************************************************
   --  Special string-catenation functions that will trim a trailing
   --  nul from the left string, if present, and will add a trailing nul
   --  to the result, if not present at the end of the right string.
   --  The easiest way to use this is with a local renaming, e.g.
   --   function "&" (Left, Right : String) return String renames Win32.Cat;
   --  **************************************************************

   function Cat (Left, Right : String) return String;

   function Cat (Left, Right : Wide_String) return Wide_String;

   function Cat (Left, Right : CHAR_Array) return CHAR_Array;

   function Cat (Left, Right : WCHAR_Array) return WCHAR_Array;

   --  ********************************************************
   --  In C, a variable-size array is declared as a[1] or
   --  a[ANYSIZE_ARRAY], where ANYSIZE_ARRAY is defined as 1.
   --  Then it is used as if it were bigger.
   --  In Ada we declare it as array (0..ANYSIZE_ARRAY) and then
   --  use the extensible array package.
   --  In C ANYSIZE_ARRAY is 1 and in Ada it is 0.
   --  ********************************************************

   ANYSIZE_ARRAY : constant := 0;                           --  winnt.h :26

   --  ********************************************************
   --  String vectors.
   --  Use the extensible-arrays package to get one of these that is bigger.
   --  Be sure to put a null pointer as the last array element.
   --  ********************************************************
   type String_Vector is array (0 .. Win32.ANYSIZE_ARRAY) of aliased
     Win32.PSTR;
   type String_Vector_Access is access all String_Vector;


   --  *******************************************
   --  Bit fields inside C structs and Ada records
   --  *******************************************

   type Bits1  is range 0 .. 2**1-1;
   type Bits2  is range 0 .. 2**2-1;
   type Bits3  is range 0 .. 2**3-1;
   type Bits4  is range 0 .. 2**4-1;
   type Bits5  is range 0 .. 2**5-1;
   type Bits6  is range 0 .. 2**6-1;
   type Bits7  is range 0 .. 2**7-1;
   type Bits8  is range 0 .. 2**8-1;
   type Bits9  is range 0 .. 2**9-1;
   type Bits10 is range 0 .. 2**10-1;
   type Bits11 is range 0 .. 2**11-1;
   type Bits12 is range 0 .. 2**12-1;
   type Bits13 is range 0 .. 2**13-1;
   type Bits14 is range 0 .. 2**14-1;
   type Bits15 is range 0 .. 2**15-1;
   type Bits16 is range 0 .. 2**16-1;
   type Bits17 is range 0 .. 2**17-1;
   type Bits18 is range 0 .. 2**18-1;
   type Bits19 is range 0 .. 2**19-1;
   type Bits20 is range 0 .. 2**20-1;
   type Bits21 is range 0 .. 2**21-1;
   type Bits22 is range 0 .. 2**22-1;
   type Bits23 is range 0 .. 2**23-1;
   type Bits24 is range 0 .. 2**24-1;
   type Bits25 is range 0 .. 2**25-1;
   type Bits26 is range 0 .. 2**26-1;
   type Bits27 is range 0 .. 2**27-1;
   type Bits28 is range 0 .. 2**28-1;
   type Bits29 is range 0 .. 2**29-1;
   type Bits30 is range 0 .. 2**30-1;
   type Bits31 is range 0 .. 2**31-1;

   for Bits1'Size use 1;
   for Bits2'Size use 2;
   for Bits3'Size use 3;
   for Bits4'Size use 4;
   for Bits5'Size use 5;
   for Bits6'Size use 6;
   for Bits7'Size use 7;
   for Bits8'Size use 8;
   for Bits9'Size use 9;
   for Bits10'Size use 10;
   for Bits11'Size use 11;
   for Bits12'Size use 12;
   for Bits13'Size use 13;
   for Bits14'Size use 14;
   for Bits15'Size use 15;
   for Bits16'Size use 16;
   for Bits17'Size use 17;
   for Bits18'Size use 18;
   for Bits19'Size use 19;
   for Bits20'Size use 20;
   for Bits21'Size use 21;
   for Bits22'Size use 22;
   for Bits23'Size use 23;
   for Bits24'Size use 24;
   for Bits25'Size use 25;
   for Bits26'Size use 26;
   for Bits27'Size use 27;
   for Bits28'Size use 28;
   for Bits29'Size use 29;
   for Bits30'Size use 30;
   for Bits31'Size use 31;

private

   pragma Inline (To_C);
   pragma Inline (To_Win);
   pragma Inline (To_Chars_Ptr);
   pragma Inline (To_Chars_Ptr);
   pragma Inline (To_PSTR);
   pragma Inline (To_PCSTR);

   --  Warning : it is endian dependent to assume which word is least
   --  significant
   type LONGLONG is record
      A, B : INT;
   end record;

   type DWORDLONG is record
      A, B : UINT;
   end record;

   ----------------------------------------------------------------------------
   --
   --  THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS PROVIDED "AS IS"
   --  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED,
   --  INCLUDING BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF
   --  MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.  The
   --  user assumes the entire risk as to the accuracy and the use of
   --  this file.  This file may be used only by licensees of
   --  Microsoft Corporation's WIN32 Software Development Kit in
   --  accordance with the terms of the licensee's End-User License
   --  Agreement for Microsoft Software for the WIN32 Development Kit.
   --
   --  Copyright (c) Intermetrics, Inc. 1995
   --  Portions (c) 1985-1994 Microsoft Corporation with permission.
   --  Microsoft is a registered trademark and Windows and Windows NT are
   --  trademarks of Microsoft Corporation.
   --
   ----------------------------------------------------------------------------

end Win32;
