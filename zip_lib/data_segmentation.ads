--  Data_Segmentation
---------------------
--
--  Pure Ada 2012+ code, 100% portable: OS-, CPU- and compiler- independent.
--
--  The Data_Segmentation package offers tools for splitting data into more
--  homogeneous blocks, with the hope of compressing those blocks better.
--
--  Legal licensing note:
--
--  Copyright (c) 2025 Gautier de Montmollin
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Ada.Containers.Vectors;

generic

  type Index is range <>;
  type Alphabet is (<>);  --  Any discrete type (usually: Byte, Unsigned_8).
  type Buffer_Type is array (Index range <>) of Alphabet;

  discrepancy_threshold : Float;  --  Discrepancy detection threshold (e.g.: 1.9).
  index_threshold       : Index;  --  Do segmentation only above a certain distance (e.g.: 20_000).
  window_size           : Index;  --  Sliding window size (e.g.: 10_000).

package Data_Segmentation is

  subtype Positive_Index is Index range 1 .. Index'Last;
  package Index_Vectors is new Ada.Containers.Vectors (Positive, Positive_Index);
  subtype Segmentation is Index_Vectors.Vector;

  procedure Segment_by_Entropy (buffer : in Buffer_Type; seg : out Segmentation);

end Data_Segmentation;
