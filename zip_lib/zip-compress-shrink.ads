--  Legal licensing note:

--  Copyright (c) 2006 .. 2020 Gautier de Montmollin
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
--
--  Translated by P2Ada (v. 15-Nov-2006)
--  from R. P. Byrne's Shrink.Pas (v. 1.2, 1989) in Turbo Pascal
--  and 16-bit assembler, then reworked by G. de Montmollin
--
--  Note about the LZW patent: as on 10-Dec-2007, one could read on
--  http://www.unisys.com/about__unisys/lzw :
--
--  Unisys U.S. LZW Patent No. 4,558,302 expired on June 20, 2003,
--  the counterpart patents in the United Kingdom, France, Germany
--  and Italy expired on June 18, 2004,
--  the Japanese counterpart patents expired on June 20, 2004
--  and the counterpart Canadian patent expired on July 7, 2004.
--

with Zip.CRC_Crypto;                    use Zip.CRC_Crypto;

private procedure Zip.Compress.Shrink (
  input,
  output           : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known : Boolean;
  input_size       : Zip_32_Data_Size_Type; -- ignored if unknown
  feedback         : Feedback_proc;
  CRC              : in out Interfaces.Unsigned_32; -- only updated here
  crypto           : in out Crypto_pack;
  output_size      : out Zip_32_Data_Size_Type;
  compression_ok   : out Boolean -- indicates compressed <= uncompressed
);
