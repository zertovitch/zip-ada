--  Data_Segmentation
---------------------
--
--  Pure Ada 2005+ code, 100% portable: OS-, CPU- and compiler- independent.
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

with Ada.Numerics.Generic_Elementary_Functions;

package body Data_Segmentation is

  procedure Segment_by_Entropy (buffer : in Buffer_Type; seg : out Segmentation) is
    --  Adapted from extras/entropy_segmentation.adb
    type Real is digits 15;
    package REF is new Ada.Numerics.Generic_Elementary_Functions (Real);
    use REF;
    inv_window_size : constant Real := 1.0 / Real (window_size);
    len : constant Index := Index (buffer'Length);
    seg_point : Index;
    freq : array (Alphabet) of Natural := (others => 0);
    elem : array (Alphabet) of Real    := (others => 0.0);
    function Entropy_Function (p : Real) return Real is (-p * Log (p));
    function Prob (b : Alphabet) return Real is (Real (freq (b)) * inv_window_size);
    entropy : Real := 0.0;
    entropy_mark : Real;
    index_mark : Index := 1;
    p : Real;
    bt : Alphabet;
  begin
    seg.Clear;
    if len > window_size + index_threshold then
      for i in 1 .. len loop
        --  Fill the sliding window.
        bt := buffer (i);
        freq (bt) := freq (bt) + 1;
        if i = window_size then
          --  Compute initial entropy value.
          for b in Alphabet loop
            p := Prob (b);
            if p > 0.0 then
              elem (b) := Entropy_Function (p);
              entropy := entropy + elem (b);
            end if;
          end loop;
          entropy_mark := entropy;
        elsif i > window_size then
          --  Adjust entropy for new value coming in.
          entropy := entropy - elem (bt);
          p := Prob (bt);
          --  Note: count (bt) is positive, px too.
          elem (bt) := Entropy_Function (p);
          entropy := entropy + elem (bt);
          --  Adjust entropy for old value disappearing.
          bt := buffer (i - window_size);
          entropy := entropy - elem (bt);
          freq (bt) := freq (bt) - 1;
          p := Prob (bt);
          if p > 0.0 then
            elem (bt) := Entropy_Function (p);
            entropy := entropy + elem (bt);
          else
            elem (bt) := 0.0;
          end if;
          if abs (entropy - entropy_mark) > Real (discrepancy_threshold) then
            seg_point := i - window_size;
            if seg_point - index_mark > index_threshold then
              seg.Append (seg_point);
              index_mark := seg_point;
              entropy_mark := entropy;
            end if;
          end if;
        end if;
      end loop;
    end if;
    if len > 0 then
      seg.Append (len);
    end if;
  end Segment_by_Entropy;

end Data_Segmentation;
