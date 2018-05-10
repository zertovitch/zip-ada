--
--  DRAFT - NOT YET FUNCTIONAL!
--
--  2nd port attempt, completely from PpmdSharp.
--

--  PPMd library
----------------

with Interfaces;
with System;

package PPMd is

  --  The compression and decompression procedures are located
  --  in child packages PPMd.Encoding and PPMd.Decoding respectively.

  Signature    : constant := 16#84acaf8f#;
  Variant      : constant Character := 'I';
  MaximumOrder : constant := 16;  --  Maximum allowed model order

private

  use Interfaces;

  subtype Int32 is Integer_32;
  subtype Byte is Unsigned_8;
  subtype UInt16 is Unsigned_16;
  subtype ushort is UInt16;  --  !!  Check this !!  Simplify to UInt(whatever)
  subtype UInt32 is Unsigned_32;
  type Unsigned is mod 2 ** System.Word_Size;
  subtype uint is UInt32;  --  !!  Check this !!  Simplify to U(whatever)

  UpperFrequency   : constant :=  5;
  IntervalBitCount : constant :=  7;
  PeriodBitCount   : constant :=  7;
  TotalBitCount    : constant :=  IntervalBitCount + PeriodBitCount;
  Interval         : constant :=  2 ** IntervalBitCount;
  BinaryScale      : constant :=  2 ** TotalBitCount;
  MaximumFrequency : constant :=  124;
  OrderBound       : constant :=  9;

  --
  --  From: See2Context.cs
  --  SEE2 (secondary escape estimation) contexts for PPM contexts with masked symbols.
  --

  type See2Context is record
    Summary      : ushort;
    Shift, Count : Byte;
  end record;

  procedure Initialize (self : in out See2Context; initialValue : uint);
  procedure Mean       (self : in out See2Context; result : out uint);
  procedure Update     (self : in out See2Context);

  --  From: Model.cs

  type Model is record
    dummy : Integer;
  end record;

end PPMd;
