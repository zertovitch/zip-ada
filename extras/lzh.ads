--  *CAUTION* : bug on the last decoded byte (see "BUG" below)
---------------
--
--  Generic LZH compression and decompression Ada 95 package.
--
--    Based on the famous LZHUF by H. OKUMURA & H. YOSHIZAKI
--    Read package body for details (including trailing byte BUG !).
--
--  Legal licensing note:
--
--  Copyright (c) 1999 .. 2009 Gautier de Montmollin (maintainer of the Ada version)
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
--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Interfaces; use Interfaces;

generic

  --  Input:
  with function  Read_byte return Unsigned_8;
  with function  More_bytes return Boolean;
  --  Output:
  with procedure Write_byte (b : Unsigned_8);

  --  NB: when the encoding length can be determined, a feedback information
  --  (percents compressed/decompressed) can be achieved in Read_byte.

package LZH is
  procedure Encode;
  procedure Decode;
end LZH;
