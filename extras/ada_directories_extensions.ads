--  (c) Martin M. Dowie, 2003-2004
--
--  This is a non-standard extension to the approved package. It is
--  currently implemented for Win32 only.
--
--  GdM 2009:
--  - Added Directory_Separator (cf GNAT.OS_Lib.Directory_Separator)
--  - Moved Validate, which is not in Ada 2005's Ada.Directories
--  - Renamed with '_' to compile with the "true" GNAT Ada.Directories.

pragma License (Modified_GPL);

with Ada.Directories; use Ada.Directories; -- ...

with Ada.Calendar;

package Ada_Directories_Extensions is

   Directory_Separator: constant Character:= '\';

   Not_Supported : exception;
   --  Can be because the underlying OS does not support this feature
   --  or because the implementation does not yet support this feature.

   procedure Touch_Modification_Time
      (Directory_Entry : in Directory_Entry_Type);
   --  Sets the modification of the file specified by <EM>Directory_Entry</EM>
   --  to the current system time. Raises Name_Error if the file does not
   --  exist or Use_Error if <EM>Name</EM> is an invalid directory
   --  entry.

   procedure Touch_Modification_Time (Name : in String);
   --  Sets the modification of the file specified by <EM>Name</EM> to
   --  the current system time. Raises Name_Error if the file does not
   --  exist or Use_Error if <EM>Name</EM> is an invalid directory
   --  entry.

   procedure Set_Access_Time (Directory_Entry : in Directory_Entry_Type;
                              To : in Ada.Calendar.Time);
   --  Set the access time of the file specified by <EM>Directory_Entry</EM> to
   --  the value <EM>To</EM>. Raises Name_Error if the file does not
   --  exist or Use_Error if <EM>Name</EM> is an invalid directory
   --  entry.

   procedure Set_Access_Time (Name : in String;
                              To : in Ada.Calendar.Time);
   --  Set the access time of the file specified by <EM>Name</EM> to
   --  the value <EM>To</EM>. Raises Name_Error if the file does not
   --  exist or Use_Error if <EM>Name</EM> is an invalid directory
   --  entry.

   procedure Set_Modification_Time (Directory_Entry : in Directory_Entry_Type;
                                    To : in Ada.Calendar.Time);
   --  Set the modification time of the file specified by
   --  <EM>Directory_Entry</EM> to the value <EM>To</EM>. Raises Name_Error if
   --  the file does not exist or Use_Error if <EM>Name</EM> is an invalid
   --  directory entry.

   procedure Set_Modification_Time (Name : in String;
                                    To : in Ada.Calendar.Time);
   --  Set the modification time of the file specified by <EM>Name</EM>
   --  to the value <EM>To</EM>. Raises Name_Error if the file does not
   --  exist or Use_Error if <EM>Name</EM> is an invalid directory
   --  entry.

   procedure Set_Times (Directory_Entry : in Directory_Entry_Type;
                        Access_Time : in Ada.Calendar.Time;
                        Modification_Time : in Ada.Calendar.Time);
   --  Set the access and modification times of the file specified by
   --  <EM>Directory_Entry</EM> to the values <EM>Access_Time</EM> and
   --  <EM>Modification_Time</EM>. Raises Name_Error if the file does
   --  not exist or Use_Error if <EM>Name</EM> is an invalid directory
   --  entry.

   procedure Set_Times (Name : in String;
                        Access_Time : in Ada.Calendar.Time;
                        Modification_Time : in Ada.Calendar.Time);
   --  Set the access and modification times of the file specified by
   --  <EM>Name</EM> to the values <EM>Access_Time</EM> and
   --  <EM>Modification_Time</EM>. Raises Name_Error if the file does
   --  not exist or Use_Error if <EM>Name</EM> is an invalid directory
   --  entry.

   --------------
   -- Validate --
   --------------

   procedure Validate (Name : in Directory_Entry_Type);

   --------------
   -- Validate --
   --------------

   procedure Validate (Name : in String);

end Ada_Directories_Extensions;
