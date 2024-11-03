--  Huffman_Encoding
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

package Huffman_Encoding is

  invalid : constant := -1;

  subtype Huffman_Code_Range is Integer range invalid .. Integer'Last;

  type Length_Code_Pair is record
    bit_length : Natural;                        --  Huffman code length, in bits
    code       : Huffman_Code_Range := invalid;  --  The code itself (the 0's and 1's)
  end record;

  type Huffman_Descriptor is array (Natural range <>) of Length_Code_Pair;

  --  The Prepare_Huffman_codes procedure finds the Huffman
  --  code for each value, given the bit_length imposed as input.

  procedure Prepare_Huffman_Codes (hd : in out Huffman_Descriptor);

end Huffman_Encoding;
