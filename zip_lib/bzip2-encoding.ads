--  BZip2.Encoding - a standalone, generic BZip2 encoder.
------------------
--
--  Examples of use:
--    BZip2_Enc, a standalone encoder to .bz2 files
--    Zip.Compress.BZip2_E, creates Zip files entries with BZip2 encoding

--  Legal licensing note:

--  Copyright (c) 2024 Gautier de Montmollin
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

package BZip2.Encoding is

  type Compression_Option is
    (block_100k,
     block_400k,
     block_900k);

  generic
    --  Input of data:
    with function  Read_Byte return Byte;
    with function  More_Bytes return Boolean;
    --  Output of LZMA-compressed data:
    with procedure Write_Byte (b : Byte);
    --
  procedure Encode (option : Compression_Option := block_900k);

end BZip2.Encoding;
