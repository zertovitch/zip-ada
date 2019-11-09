--  UnZip.Decompress.Huffman
----------------------------
--  Huffman tree generation and deletion.
--  Originally from info-zip's unzip, data structure rewritten by G. de Montmollin

--  Legal licensing note:

--  Copyright (c) 1999 .. 2019 Gautier de Montmollin
--  SWITZERLAND

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

--  NB: this is the MIT License, as found on the site
--  http://www.opensource.org/licenses/mit-license.php

private package UnZip.Decompress.Huffman is

  type HufT_table;
  type p_HufT_table is access HufT_table;

  type HufT is record
    extra_bits : Natural;
    bits       : Natural;
    n          : Natural;
    next_table : p_HufT_table;
  end record;

  invalid : constant := 99; -- invalid value for extra bits

  type HufT_table is array (Natural range <>) of HufT;

  --  Linked list just for destroying Huffman tables

  type Table_list;
  type p_Table_list is access Table_list;

  type Table_list is record
    table : p_HufT_table;
    next  : p_Table_list;
  end record;

  type Length_array is array (Integer range <>) of Natural;

  empty : constant Length_array (1 .. 0) := (others => 0);

  --  Free huffman tables starting with table where t points to
  procedure HufT_free (tl : in out p_Table_list);

  --  Build huffman table from code lengths given by array b.all
  procedure HufT_build (b    : Length_array;
                        s    : Integer;
                        d, e : Length_array;
                        tl   :    out p_Table_list;
                        m    : in out Integer;
             huft_incomplete :    out Boolean);

  --  Possible exceptions occuring in huft_build
  huft_error,                    -- bad tree constructed
  huft_out_of_memory : exception; -- not enough memory

end UnZip.Decompress.Huffman;
