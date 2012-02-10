-------------------------------------------------------------------------------
--
--  THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS PROVIDED WITHOUT CHARGE
--  "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
--  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR
--  FITNESS FOR A PARTICULAR PURPOSE.  The user assumes the entire risk as to
--  the accuracy and the use of this file.  This file may be used, copied,
--  modified and distributed only by licensees of Microsoft Corporation's
--  WIN32 Software Development Kit in accordance with the terms of the
--  licensee's End-User License Agreement for Microsoft Software for the
--  WIN32 Development Kit.
--
--  Copyright (c) Intermetrics, Inc. 1995
--  Portions (c) 1985-1994 Microsoft Corporation with permission.
--  Microsoft is a registered trademark and Windows and Windows NT are
--  trademarks of Microsoft Corporation.
--
-------------------------------------------------------------------------------

with Win32.crt.Types;

package Win32.crt.Stat is

   S_IFMT : constant := 8#170000#; --  stat.h:81
   S_IFDIR : constant := 8#40000#;  --  stat.h:82
   S_IFCHR : constant := 8#20000#;  --  stat.h:83
   S_IFIFO : constant := 8#10000#;  --  stat.h:84
   S_IFREG : constant := 8#100000#; --  stat.h:85
   S_IREAD : constant := 8#400#;    --  stat.h:86
   S_IWRITE : constant := 8#200#;    --  stat.h:87
   S_IEXEC : constant := 8#100#;    --  stat.h:88

   type struct_stat is                                     --  stat.h:60
      record
         st_dev : Win32.crt.Types.dev_t;                --  stat.h:61
         st_ino : Win32.crt.Types.ino_t;                --  stat.h:62
         st_mode : Win32.USHORT;                         --  stat.h:63
         st_nlink : Win32.SHORT;                          --  stat.h:64
         st_uid : Win32.SHORT;                          --  stat.h:65
         st_gid : Win32.SHORT;                          --  stat.h:66
         st_rdev : Win32.crt.Types.dev_t;                --  stat.h:67
         st_size : Win32.crt.Types.off_t;                --  stat.h:68
         st_atime : Win32.crt.Types.time_t;               --  stat.h:69
         st_mtime : Win32.crt.Types.time_t;               --  stat.h:70
         st_ctime : Win32.crt.Types.time_t;               --  stat.h:71
      end record;

   function fstat (handle : Win32.INT;
                   buffer : access struct_stat)
                  return Win32.INT;                    --  stat.h:93

   function stat (path : Win32.PCSTR;
                  buffer : access struct_stat)
                 return Win32.INT;                     --  stat.h:94

private

   pragma Convention (C, struct_stat);                      --  stat.h:60

   pragma Import (C, fstat, "_fstat");                      --  stat.h:93
   pragma Import (C, stat, "_stat");                        --  stat.h:94

end Win32.crt.Stat;


