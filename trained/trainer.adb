--  Trainer
-------------------
--  This is a demo showing the creation of the compressed trainer
--  data for Trained_Compression, to be used on the decoding side.
--
--  This is almost the same as using the trained encoder with a zero
--  trainer file, but the compressed trainer stub is encoded without
--  end-of-stream marker, which makes more predictable the length
--  of the compressed stream which is common between trainer only
--  and trainer+data.
--
--  See the trtest_single.cmd script for an example
--
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

with Trained_Compression;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Interfaces;

procedure Trainer is

  --  NB: The Byte I/O below is not buffered, so it is very slow.
  --  You need to implement a circular buffer of type Stream_Element_Array for a fast I/O.
  --  For instance, see the BlockRead in the Zip package for how to do it.

  subtype Byte is Interfaces.Unsigned_8;
  package Byte_IO is new Ada.Direct_IO (Byte);

  Infile, Outfile : Byte_IO.File_Type;

  function Read_Byte return Byte is
    B : Byte;
  begin
    Byte_IO.Read (Infile, B);
    return B;
  end Read_Byte;

  function More_Bytes return Boolean is
  begin
    return not Byte_IO.End_Of_File (Infile);
  end More_Bytes;

  procedure Write_Byte (B : Byte) is
  begin
    Byte_IO.Write (Outfile, B);
  end Write_Byte;

  procedure TCT is new Trained_Compression.Encode_Trainer (
    Read_Uncompressed       => Read_Byte,
    More_Uncompressed_Bytes => More_Bytes,
    Write_Compressed_Byte   => Write_Byte);

begin
  if Argument_Count < 2 then
    Put_Line ("Syntax:");
    Put_Line ("trainer train_file (in) compressed_train_file (out)");
  else
    Byte_IO.Open (Infile,    Byte_IO.In_File,  Name => Argument (1));
    Byte_IO.Create (Outfile, Byte_IO.Out_File, Name => Argument (2));
    TCT;
    Byte_IO.Close (Infile);
    Byte_IO.Close (Outfile);
  end if;
end Trainer;
