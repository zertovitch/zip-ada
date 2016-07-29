--  Items that are common to LZMA encoding and LZMA decoding.

with Interfaces;
with System;

package LZMA is

  --  Nothing public so far...

private

  use Interfaces;

  --  These types are defined in the LZMA specification
  --  (DRAFT version, 2013-07-28, by Igor Pavlov)

  subtype Byte is Unsigned_8;
  subtype UInt16 is Unsigned_16;
  subtype UInt32 is Unsigned_32;
  type Unsigned is mod 2 ** System.Word_Size;

  subtype LC_range is Integer range 0..8;
  subtype LP_range is Integer range 0..4;
  subtype PB_range is Integer range 0..4;

end LZMA;
