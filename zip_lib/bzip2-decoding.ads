--  BZip2.Decoding - a standalone, generic BZip2 decoding package.
------------------
--
--  Legal licensing note:
--
--  Copyright (c) 2009 .. 2019 Gautier de Montmollin (maintainer of the Ada version)
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

--------------------------------------------------------------
--  This Ada code is a reworked translation of a Pascal version,
--  by Daniel Mantione, of the decompression code of libbzip2
--  by Julian Seward.
--
--  Translated on 20-Oct-2009 by (New) P2Ada v. 15-Nov-2006
--  Rework by G. de Montmollin
--
--  Main difference over the FreePascal version: there is no more pointer
--  arithmetics. The only pointer is tt, for dynamically allocating the biggest
--  decoding array.
--  With the appropriate options, the performance is very close to
--  the bzip2 tool in C: it takes around 7%-11% more time depending on data
--  to be decompressed (tested in 2009). Add some 5% when CRC checking is enabled.
--  These timings are obtained with bunzip.adb compiled on GNAT 2008, Win32,
--  with the -O2 -gnatpn -fpeel-loops -funroll-loops -fweb -frename-registers
--  options, average on several runs (see bz_test.cmd).

generic

  input_buffer_size  : Integer := 1024;
  output_buffer_size : Integer := 4096;

  type Buffer is array (Natural range <>) of Interfaces.Unsigned_8;

  --  CRC checking is useless if the whole bzip stream is enclosed
  --  in another CRC-checked stream, like a in Zip archive.
  check_CRC : Boolean;

  --  Input:
  with procedure Read (buf : out Buffer);
  --  Output:
  with procedure Write (buf : in Buffer);

package BZip2.Decoding is

  bad_header_magic,
  bad_block_magic,
  data_error,
  block_crc_check_failed,
  randomized_not_yet_implemented : exception;

  --  Decompress: decompression of bzip2 data streams.
  procedure Decompress;

end BZip2.Decoding;
