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

package Win32.crt.Time is

   CLOCKS_PER_SEC : constant := 1000;
   --  time.h:92

   type clock_t is new Win32.LONG;                         --  time.h:53

   type tm;                                                --  time.h:75

   type ac_time_t is access constant Win32.crt.Types.time_t;
   type ac_tm_t is access constant tm;                     --  time.h:137
   type a_tm_t is access all tm;                           --  time.h:141

   type tm is                                              --  time.h:75
      record
         tm_sec : Win32.INT;                            --  time.h:76
         tm_min : Win32.INT;                            --  time.h:77
         tm_hour : Win32.INT;                            --  time.h:78
         tm_mday : Win32.INT;                            --  time.h:79
         tm_mon : Win32.INT;                            --  time.h:80
         tm_year : Win32.INT;                            --  time.h:81
         tm_wday : Win32.INT;                            --  time.h:82
         tm_yday : Win32.INT;                            --  time.h:83
         tm_isdst : Win32.INT;                            --  time.h:84
      end record;

   type TZ_Array is array (0 .. 1) of Win32.PSTR;

   --  auxiliary declarations
   type AI is access Win32.INT;
   function daylight_Addr return AI;
   pragma Import (C, daylight_Addr, "__p__daylight");

   type AL is access Win32.LONG;
   function timezone_Addr return AL;
   pragma Import (C, timezone_Addr, "__p__timezone");

   type ATZ is access all TZ_Array;
   function tzname_Addr return ATZ;
   pragma Import (C, tzname_Addr, "__p__tzname");
   --  end of auxiliary declarations

   daylight : Win32.INT renames daylight_Addr.all;          --  time.h:101
   --  non-zero if daylight savings time is used

   timezone : Win32.LONG renames timezone_Addr.all;         --  time.h:102
   --  difference in seconds between GMT and local time

   tzname : TZ_Array renames tzname_Addr.all;               --  time.h:111
   --  standard/daylight savings time zone names

   function asctime (timeptr : ac_tm_t) return Win32.PSTR;   --  time.h:137

   function ctime (timer : ac_time_t) return Win32.PSTR;     --  time.h:138

   function clock return clock_t;                          --  time.h:139

   function difftime (timer1 : Win32.crt.Types.time_t;
                      timer0 : Win32.crt.Types.time_t)
                     return Win32.DOUBLE;              --  time.h:140

   function gmtime (timer : ac_time_t) return a_tm_t;        --  time.h:141

   function localtime (timer : ac_time_t) return a_tm_t;     --  time.h:142

   function mktime (timeptr : access tm) return Win32.crt.Types.time_t;
   --  time.h:143

   function strftime (string1 : Win32.PSTR;
                      maxsize : Win32.Size_T;
                      format : Win32.PCSTR;
                      timeptr : ac_tm_t)
                     return Win32.Size_T;              --  time.h:144

   function strdate (datestr : Win32.PSTR) return Win32.PSTR;
   --  time.h:145

   function strtime (timestr : Win32.PSTR) return Win32.PSTR;
   --  time.h:146

   function time (timer : access Win32.crt.Types.time_t)
                 return Win32.crt.Types.time_t;              --  time.h:147

   procedure tzset;                                        --  time.h:151

   function getsystime (p1 : access tm) return Win32.UINT;
   --  time.h:153

   function setsystime (p1 : access tm;
                        p2 : Win32.UINT)
                       return Win32.UINT;              --  time.h:154

private

   pragma Convention (C, tm);                               --  time.h:75

   pragma Import (C, asctime, "asctime");                   --  time.h:137
   pragma Import (C, ctime, "ctime");                       --  time.h:138
   pragma Import (C, clock, "clock");                       --  time.h:139
   pragma Import (C, difftime, "difftime");                 --  time.h:140
   pragma Import (C, gmtime, "gmtime");                     --  time.h:141
   pragma Import (C, localtime, "localtime");               --  time.h:142
   pragma Import (C, mktime, "mktime");                     --  time.h:143
   pragma Import (C, strftime, "strftime");                 --  time.h:144
   pragma Import (C, strdate, "_strdate");                  --  time.h:145
   pragma Import (C, strtime, "_strtime");                  --  time.h:146
   pragma Import (C, time, "time");                         --  time.h:147
   pragma Import (C, tzset, "_tzset");                      --  time.h:151
   pragma Import (C, getsystime, "_getsystime");            --  time.h:153
   pragma Import (C, setsystime, "_setsystime");            --  time.h:154

end Win32.crt.Time;
