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

package Win32.crt.Utime is

   type utimbuf is                                         --  utime.h:55
      record
         actime : Win32.crt.Types.time_t;                --  utime.h:56
         modtime : Win32.crt.Types.time_t;                --  utime.h:57
      end record;

   function utime (p1 : Win32.PSTR;
                   p2 : access utimbuf)
                  return Win32.INT;                    --  utime.h:69

   function futime (p1 : Win32.INT;
                    p2 : access utimbuf)
                   return Win32.INT;                   --  utime.h:70

private

   pragma Convention (C_Pass_By_Copy, utimbuf);                  --  utime.h:55

   pragma Import (C, utime, "_utime");                      --  utime.h:69
   pragma Import (C, futime, "_futime");                    --  utime.h:70

end Win32.crt.Utime;
