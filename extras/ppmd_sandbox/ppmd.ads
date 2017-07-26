--
--  DRAFT - NOT YET FUNCTIONAL!
--

--  PPMd library
----------------
--  Library for encoding and decoding data streams in the PPMd compression
--  format invented by Dmitry Subbotin and Dmitry Shkarin (PPMd var.H (2001)).
--  The Ada code is based on the recoding done by Igor Pavlov.
--  Some de-obfuscation hints and de-pointerization techniques are
--  from PpmdSharp by Michael Bone.
--  PPM means: Prediction by Partial Match.
--
--  Pure Ada 95+ code, 100% portable: OS-, CPU- and compiler- independent.

--  Legal licensing note:

--  Copyright (c) 2017 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Interfaces;
with System;

package PPMd is

  --  The compression and decompression procedures are located
  --  in child packages PPMd.Encoding and PPMd.Decoding respectively.

private

  use Interfaces;

  subtype Int32 is Integer_32;
  subtype Byte is Unsigned_8;
  subtype UInt16 is Unsigned_16;
  subtype UInt32 is Unsigned_32;
  type Unsigned is mod 2 ** System.Word_Size;

  --  SEE-contexts for PPM-contexts with masked symbols.
  --  SEE means: Secondary Escape Estimation.

  --  !! de-obfuscation: summ = summary

  type CPpmd_See is record
    Summ  : UInt16;  --  Freq
    Shift : Byte;    --  Speed of Freq change; low Shift is for fast change
    Count : Byte;    --  Count to next change of Shift
  end record;

  procedure Ppmd_See_Update (p : in out CPpmd_See);
  pragma Inline (Ppmd_See_Update);

  type CPpmd_State is record
    Symbol        : Byte;
    Freq          : Byte;
    SuccessorLow  : UInt16;
    SuccessorHigh : UInt16;
  end record;

  subtype Big_mem_index is UInt32;

  subtype CPpmd_State_Ref is Big_mem_index;
  subtype CPpmd_Void_Ref  is Big_mem_index;
  subtype CPpmd_Byte_Ref  is Big_mem_index;

  type CPpmd7_Context;

  type CPpmd7_Context_Ref is access CPpmd7_Context;

  type CPpmd7_Context is record
    NumStats : UInt16;
    SummFreq : UInt16;
    Stats    : CPpmd_State_Ref;
    Suffix   : CPpmd7_Context_Ref;
  end record;

  PPMD_N1 : constant := 4;
  PPMD_N2 : constant := 4;
  PPMD_N3 : constant := 4;
  PPMD_N4 : constant := ((128 + 3 - 1 * PPMD_N1 - 2 * PPMD_N2 - 3 * PPMD_N3) / 4);
  PPMD_NUM_INDEXES : constant := (PPMD_N1 + PPMD_N2 + PPMD_N3 + PPMD_N4);

  type Index_to_unit_array is array (Unsigned'(0) .. PPMD_NUM_INDEXES - 1) of Byte;
  type Unit_to_index_array is array (Unsigned'(0) .. 127) of Byte;
  type Free_list_array is array (Unsigned'(0) .. PPMD_NUM_INDEXES - 1) of CPpmd_Void_Ref;
  type NS_BS_HB_array is array (0 .. 255) of Byte;
  type See_array is array (0 .. 24, 0 .. 15) of CPpmd_See;
  type Bin_summ_array is array (0 .. 127, 0 .. 63) of UInt16;

  --  PPMd uses a large memory chunk and defines its own memory
  --  management (allocate, free, ...) within it.

  type Big_mem_array is array (Big_mem_index range <>) of Byte;
  type Big_mem_array_access is access Big_mem_array;

  type CPpmd7 is record
    MinContext, MaxContext : CPpmd7_Context_Ref;
    FoundState             : CPpmd_State_Ref;
    OrderFall, InitEsc,
    PrevSuccess,
    MaxOrder, HiBitsFlag   : Unsigned;
    RunLength, InitRL      : Int32;  --  /* must be 32-bit at least */ !! Integer_M32 ?
    Base                   : Big_mem_array_access := null;
    Size                   : UInt32 := 0;
    GlueCount              : UInt32;
    LoUnit, HiUnit,
    Text, UnitsStart       : CPpmd_Byte_Ref;
    AlignOffset            : UInt32;
    Indx2Units             : Index_to_unit_array;
    Units2Indx             : Unit_to_index_array;
    FreeList               : Free_list_array;
    NS2Indx,
    NS2BSIndx,                                  --  NumberStatistics To BinarySummary
    HB2Flag                : NS_BS_HB_array;
    DummySee, See          : See_array;
    BinSumm                : Bin_summ_array;
  end record;

  ----------------------
  --  Range encoding  --
  ----------------------

  --  Normalization threshold.
  --  When the range width is below that value, a shift is needed.
  --
  width_threshold : constant := 2**24;  --  7z name: "kTopValue"

  --  The following article explains how range encoding works:
  --
  --     G. N. N. Martin, Range encoding: an algorithm for removing redundancy
  --     from a digitized message, Video & Data Recording Conference,
  --     Southampton, UK, July 24-27, 1979.

end PPMd;
