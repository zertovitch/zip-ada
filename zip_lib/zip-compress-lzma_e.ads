--  Legal licensing note:

--  Copyright (c) 2016 .. 2020 Gautier de Montmollin
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

with Zip.CRC_Crypto;                    use Zip.CRC_Crypto;

private procedure Zip.Compress.LZMA_E (
  input,
  output           : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known : Boolean;
  input_size       : Zip_32_Data_Size_Type; -- ignored if unknown
  feedback         : Feedback_proc;
  method           : LZMA_Method;
  CRC              : in out Interfaces.Unsigned_32; -- only updated here
  crypto           : in out Crypto_pack;
  output_size      : out Zip_32_Data_Size_Type;
  compression_ok   : out Boolean -- indicates when compressed <= uncompressed
);
