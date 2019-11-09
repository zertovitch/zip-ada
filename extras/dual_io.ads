------------------------------------------------------------------------------
--  File:            Dual_IO.ads
--  Description:     Dual_IO : clones the Text_IO I/O functions towards
--                   Standard I/O but also outputs these I/O to a log file.
--
--  NB:  1) Use Create_Log and Close_Log to create and close a log file.
--       2) Generic Integer_IO, Float_IO, ... are of course also present.
--       3) Only procedures for I/O to Standard device are kept, use the
--          genuine Text_IO for Files and String I/O.
--
--
--  Date/version:    2-Mar-2013; 2-Feb-2011; 7-Jul-2001; 4-Jul-2001
--  Author:          G. de Montmollin
--                   http://gautiersblog.blogspot.com/
------------------------------------------------------------------------------

with IO_Exceptions, Text_IO;
   --  ^ These are standard renamings of Ada.Text_IO & Ada.IO_Exceptions

package Dual_IO is

   subtype Count is Text_IO.Count;

   subtype Positive_Count is Text_IO.Positive_Count;

   Unbounded : constant Count := Text_IO.Unbounded;

   subtype Field is Text_IO.Field;

   subtype Number_Base is Text_IO.Number_Base;

   subtype Type_Set is Text_IO.Type_Set;

   -------------------------
   -- Log file Management --
   -------------------------

   procedure Create_Log (Name : in String);
   procedure Append_Log (Name : in String);
   procedure Close_Log;
   function Is_Log_Open return Boolean;

   --  Close and reopen: have an up to date copy on file system
   procedure Close_and_Append_Log;

   --  Buffer control
   procedure Flush;

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   procedure Set_Line_Length (To : in Count) renames Text_IO.Set_Line_Length;
   procedure Set_Page_Length (To : in Count) renames Text_IO.Set_Page_Length;
   function Line_Length return Count renames Text_IO.Line_Length;
   function Page_Length return Count renames Text_IO.Page_Length;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------

   procedure New_Line (Spacing : in Positive_Count := 1);
   procedure Skip_Line (Spacing : in Positive_Count := 1);
   function End_Of_Line return Boolean renames Text_IO.End_Of_Line;
   procedure New_Page;
   procedure Skip_Page;
   function End_Of_Page return Boolean         renames Text_IO.End_Of_Page;
   function End_Of_File return Boolean         renames Text_IO.End_Of_File;
   procedure Set_Col (To : in Positive_Count)  renames Text_IO.Set_Col;
   procedure Set_Line (To : in Positive_Count) renames Text_IO.Set_Line;
   function Col return Positive_Count          renames Text_IO.Col;
   function Line return Positive_Count         renames Text_IO.Line;
   function Page return Positive_Count         renames Text_IO.Page;

   -----------------------------
   -- Characters Input-Output --
   -----------------------------

   procedure Get (Item : out Character);
   procedure Put (Item : in Character);

   procedure Look_Ahead (Item           : out Character;
                         Is_End_Of_Line : out Boolean)
      renames Text_IO.Look_Ahead;

   --  No echo -> not logged -> renames suffices

   procedure Get_Immediate (Item      : out Character)
      renames Text_IO.Get_Immediate;

   procedure Get_Immediate (Item      : out Character;
                            Available : out Boolean)
      renames Text_IO.Get_Immediate;

   --------------------------
   -- Strings Input-Output --
   --------------------------

   procedure Get (Item : out String);
   procedure Put (Item : in String);

   procedure Get_Line
     (Item : out String;
      Last : out Natural);

   procedure Put_Line
     (Item : in String);

   --  Generic package for Input-Output of Integer Types

   generic
      type Num is range <>;
   package Integer_IO is

      Default_Width : Field := Num'Width;
      Default_Base  : Number_Base := 10;

      procedure Get (Item  : out Num;
                     Width : in  Field := 0);

      procedure Put (Item  : in Num;
                     Width : in Field       := Default_Width;
                     Base  : in Number_Base := Default_Base);

   end Integer_IO;

   --  Generic package for Input-Output of Real Types

   generic
      type Num is digits <>;
   package Float_IO is

      Default_Fore : Field := 2;
      Default_Aft  : Field := Num'Digits - 1;
      Default_Exp  : Field := 3;

      procedure Get (Item  : out Num;
                     Width : in  Field := 0);

      procedure Put (Item : in Num;
                     Fore : in Field := Default_Fore;
                     Aft  : in Field := Default_Aft;
                     Exp  : in Field := Default_Exp);
   end Float_IO;

   generic
      type Num is delta <>;
   package Fixed_IO is

      Default_Fore : Field := Num'Fore;
      Default_Aft  : Field := Num'Aft;
      Default_Exp  : Field := 0;

      procedure Get (Item  : out Num;
                     Width : in  Field := 0);

      procedure Put (Item : in Num;
                     Fore : in Field := Default_Fore;
                     Aft  : in Field := Default_Aft;
                     Exp  : in Field := Default_Exp);
   end Fixed_IO;

   --  Generic package for Input-Output of Decimal Types

   generic
      type Num is delta <> digits <>;

   package Decimal_IO is

      Default_Fore : Field := Num'Fore;
      Default_Aft  : Field := Num'Aft;
      Default_Exp  : Field := 0;

      procedure Get
        (Item  : out Num;
         Width : in Field := 0);

      procedure Put
        (Item : in Num;
         Fore : in Field := Default_Fore;
         Aft  : in Field := Default_Aft;
         Exp  : in Field := Default_Exp);

   end Decimal_IO;

   --  Generic package for Input-Output of Modular Types

   generic
      type Num is mod <>;

   package Modular_IO is

      Default_Width : Field := Num'Width;
      Default_Base  : Number_Base := 10;

      procedure Get
        (Item  : out Num;
         Width : in Field := 0);

      procedure Put
        (Item  : in Num;
         Width : in Field       := Default_Width;
         Base  : in Number_Base := Default_Base);

   end Modular_IO;
   --  Generic package for Input-Output of Enumeration Types

   generic
      type Enum is (<>);
   package Enumeration_IO is

      Default_Width   : Field := 0;
      Default_Setting : Type_Set := Text_IO.Upper_Case;

      procedure Get (Item : out Enum);

      procedure Put (Item  : in Enum;
                     Width : in Field    := Default_Width;
                     Set   : in Type_Set := Default_Setting);
   end Enumeration_IO;

   --  Exceptions

   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error   : exception renames IO_Exceptions.Mode_Error;
   Name_Error   : exception renames IO_Exceptions.Name_Error;
   Use_Error    : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error    : exception renames IO_Exceptions.End_Error;
   Data_Error   : exception renames IO_Exceptions.Data_Error;
   Layout_Error : exception renames IO_Exceptions.Layout_Error;

   Log_not_open     : exception;
   Log_already_open : exception;

end Dual_IO;
