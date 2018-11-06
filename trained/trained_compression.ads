--  Universal Trained Compression
---------------------------------
--  This package provides a generic streaming encoder-decoder
--  engine with the capability of training the engine with data known
--  in advance, in order to achieve better compression of the
--  actual data to be transmitted if the known data is similar
--  to the training data.
--  See trtest.cmd for a test and ../doc/za_work.xls (sheet: Trained)
--  for some results.
--
--  Conceptually it works like that (in the streams 1. is followed by 2.):
--
--   Encoding workflow:
--   ------------------
--
--      1. training --\    compression    /--> 1. training' (discarded)
--                     >-->----aka----->--
--      2. DATA ------/     encoding      \--> 2. DATA'  --> ...
--
--   Decoding workflow:
--   ------------------
--
--              1. training' --\   decompression  /--> 1. training (discarded)
--                              >-->---aka---->---
--      ... --> 2. DATA' ------/     decoding     \--> 2. DATA
--
-------------------------
--  Legal licensing note:
--
--  Copyright (c) 2018 Gautier de Montmollin
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

package Trained_Compression is

  subtype Byte is Interfaces.Unsigned_8;

  ---------------------------------------------
  --  Encoding - compression of the trainer  --
  ---------------------------------------------

  generic
    --  Input of trainer bytes:
    with function Read_Uncompressed return Byte;
    with function More_Uncompressed_Bytes return Boolean;
    --  Output of compressed trainer bytes:
    with procedure Write_Compressed_Byte (B : Byte);
    --
  procedure Encode_Trainer;

  -------------------------------------
  --  Encoding - compression of data --
  -------------------------------------

  generic
    type Data_Bytes_Count is range <>;
    --  Input of training or data bytes:
    with function Read_Uncompressed_Training return Byte;
    with function Read_Uncompressed_Data return Byte;
    with function More_Uncompressed_Data_Bytes return Boolean;
    --  Output of compressed data:
    with procedure Write_Compressed_Byte (B : Byte);
    --
    --  Important: Skip_Compressed needs to be slightly less
    --  than the full compressed trainer's actual size.
    --  Reason: the trainer, with data after it, may have a
    --  longer LZ77 match that the trainer alone would not have
    --  near its end. You can append a bit of noise to
    --  the uncompressed trainer data to avoid having to reduce
    --  Skip_Compressed too much.
    --
  procedure Encode (Train_Uncompressed, Skip_Compressed : Data_Bytes_Count);

  --------------------------------
  --  Decoding - decompression  --
  --------------------------------

  generic
    type Data_Bytes_Count is range <>;
    --  Input of training or data bytes:
    with function Read_Compressed_Training return Byte;
    with function Read_Compressed_Data return Byte;
    --  Output of compressed data:
    with procedure Write_Decompressed_Byte (B : Byte);
    --
    --  Important: Train_Compressed needs to be equal to
    --             the encoder's Skip_Compressed value.
    --
  procedure Decode (Train_Compressed, Skip_Decompressed : Data_Bytes_Count);

end Trained_Compression;
