--  Huffman.Encoding
--------------------
--
--  Legal licensing note:
--
--  Copyright (c) 2024 Gautier de Montmollin (maintainer of the Ada version)
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
--  NB: this is the MIT License, as found 03-Nov-2024 on the site
--  http://www.opensource.org/licenses/mit-license.php

package body Huffman.Encoding is

  use Interfaces;

  procedure Invert (lc : in out Length_Code_Pair) is
    a : Code_Range := lc.code;
    b : Code_Range := 0;
  begin
    for i in 1 .. lc.bit_length loop
      b := b * 2 + a mod 2;
      a := a / 2;
    end loop;
    lc.code := b;
  end Invert;

  procedure Prepare_Codes
    (hd               : in out Descriptor;
     max_huffman_bits : in     Positive;
     invert_bit_order : in     Boolean)
  is
    bl_count, next_code : array (0 .. max_huffman_bits) of Code_Range := (others => 0);
    code : Code_Range := 0;
    bl : Natural;
  begin
    --  Algorithm from RFC 1951, section 3.2.2.
    --  Step 1)
    for i in hd'Range loop
      bl := hd (i).bit_length;
      bl_count (bl) := bl_count (bl) + 1;  --  One more code to be defined with bit length bl
    end loop;
    --  Step 2)
    for bits in 1 .. max_huffman_bits loop
      code := (code + bl_count (bits - 1)) * 2;
      next_code (bits) := code;  --  This will be the first code for bit length "bits"
    end loop;
    --  Step 3)
    for n in hd'Range loop
      bl := hd (n).bit_length;
      if bl > 0 then
        hd (n).code := next_code (bl);
        next_code (bl) := next_code (bl) + 1;
      else
        hd (n).code := 0;
      end if;
    end loop;
    if invert_bit_order then
      for i in hd'Range loop
        Invert (hd (i));
      end loop;
    end if;
  end Prepare_Codes;

end Huffman.Encoding;
