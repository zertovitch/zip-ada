--  Length_limited_Huffman_code_lengths
---------------------------------------
--    This algorithm builds optimal Huffman codes for a given alphabet
--    and occurrence counts (frequencies) of this alphabet. These occurrences
--    are supposed to have been counted in a message to be sent in a
--    compressed form using the Huffman codes. As output, only the bit lengths
--    of the Huffman codes are given; the Huffman codes themselves are built
--    with these bit lengths when the message actually needs to be sent.
--
--  Pure Ada 95+ code, 100% portable: OS-, CPU- and compiler- independent.

--  Legal licensing note:

--  Copyright (c) 2016 .. 2019 Gautier de Montmollin (maintainer of the Ada version)
--  SWITZERLAND
--
--  The copyright holder is only the maintainer of the Ada version;
--  authors of the C code and those of the algorithm are cited below.

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

--  Author: lode.vandevenne [*] gmail [*] com (Lode Vandevenne)
--  Author: jyrki.alakuijala [*] gmail [*] com (Jyrki Alakuijala)

--  Bounded package merge algorithm, based on the paper
--    "A Fast and Space-Economical Algorithm for Length-Limited Coding
--    Jyrki Katajainen, Alistair Moffat, Andrew Turpin".

--  Translated by G. de Montmollin to Ada from katajainen.c (Zopfli project), 7-Feb-2016
--  Translation notes in procedure's body.

generic
  type Alphabet is (<>);  --  Any discrete type
  --  Count_Type is an integer type large enough for counting
  --  and indexing. See body for actual bounds.
  type Count_Type is range <>;
  type Count_Array is array (Alphabet) of Count_Type;
  type Length_Array is array (Alphabet) of Natural;
  max_bits : Positive;  --  Length limit in Huffman codes

procedure Length_limited_Huffman_code_lengths (
  frequencies : in  Count_Array;
  bit_lengths : out Length_Array
);
