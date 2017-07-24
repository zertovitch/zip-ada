--  DRAFT - NOT YET FUNCTIONAL!

--  PPMd library
----------------
--  Library for encoding and decoding data streams in the PPMd compression
--  format invented by Dmitry Subbotin and Dmitry Shkarin (PPMd var.H (2001)).
--  The Ada code is based on the recoding done by Igor Pavlov.
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

-- NB: this is the MIT License, as found 21-Aug-2016 on the site
-- http://www.opensource.org/licenses/mit-license.php

with Interfaces;
with System;

package PPMd is

private

  use Interfaces;

  subtype Byte is Unsigned_8;
  subtype UInt16 is Unsigned_16;
  subtype UInt32 is Unsigned_32;
  type Unsigned is mod 2 ** System.Word_Size;

  -- SEE-contexts for PPM-contexts with masked symbols.
  -- SEE means: Secondary Escape Estimation.

  type CPpmd_See is record
    Summ  : UInt16;  --  Freq
    Shift : Byte;    --  Speed of Freq change; low Shift is for fast change
    Count : Byte;    --  Count to next change of Shift
  end record;

  procedure Ppmd_See_Update (p: in out CPpmd_See);
  pragma Inline(Ppmd_See_Update);

  type CPpmd_State is record
    Symbol        : Byte;   
    Freq          : Byte;   
    SuccessorLow  : UInt16;
    SuccessorHigh : UInt16;
  end record;

  type void is new Integer;  --  !! get rid of that!
  
  type CPpmd_State_Ref is access CPpmd_State;  --  Alt.: UInt32 if not PPMD_32BIT
  type CPpmd_Void_Ref  is access void;         --  Alt.: UInt32 if not PPMD_32BIT
  type CPpmd_Byte_Ref  is access Byte;         --  Alt.: UInt32 if not PPMD_32BIT

  type CPpmd7_Context;
  type CPpmd7_Context_Ref is access CPpmd7_Context;
  type CPpmd7_Context is record
    NumStats : UInt16;
    SummFreq : UInt16;
    Stats    : CPpmd_State_Ref;
    Suffix   : CPpmd7_Context_Ref;
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
