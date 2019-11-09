--  BZip2
---------
--
--  Pure Ada 95+ code, 100% portable: OS-, CPU- and compiler- independent.
--
--  bzip2 compresses files using the Burrows-Wheeler block-sorting text
--  compression algorithm, and Huffman coding. Compression is generally
--  considerably better than that achieved by more conventional
--  LZ77/LZ78-based compressors, and approaches the performance of the
--  PPM family of statistical compressors.
--
--  Documentation pointers:
--
--    Burrows-Wheeler transform
--      https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
--    MTF Move-To-Front
--      https://en.wikipedia.org/wiki/Move-to-front_transform
--
--  Legal licensing note:
--
--  Copyright (c) 2018 .. 2019 Gautier de Montmollin (maintainer of the Ada version)
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

with Interfaces;

package BZip2 is

  subtype Byte is Interfaces.Unsigned_8;

private

  --  Cyclic redundancy check to verify uncompressed block data integrity

  package CRC is
    use Interfaces;
    --
    procedure Init (CRC_Value : out Unsigned_32);
    function  Final (CRC_Value : Unsigned_32) return Unsigned_32;
    procedure Update (CRC_Value : in out Unsigned_32; val : Byte);
      pragma Inline (Update);
  end CRC;

end BZip2;
