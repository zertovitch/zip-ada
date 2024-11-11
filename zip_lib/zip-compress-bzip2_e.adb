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

with BZip2.Encoding;

procedure Zip.Compress.BZip2_E
  (input,
   output           : in out Zip_Streams.Root_Zipstream_Type'Class;
   input_size_known :        Boolean;
   input_size       :        Zip_64_Data_Size_Type;   --  ignored if unknown
   feedback         :        Feedback_Proc;
   method           :        BZip2_Method;
   CRC              : in out Interfaces.Unsigned_32;  --  only updated here
   crypto           : in out CRC_Crypto.Crypto_pack;
   output_size      :    out Zip_64_Data_Size_Type;
   compression_ok   :    out Boolean)  --  indicates compressed < uncompressed
is
  use Interfaces;

  ------------------
  -- Buffered I/O --
  ------------------

  IO_buffers : IO_Buffers_Type;

  procedure Put_Byte (B : Unsigned_8) is
  begin
    IO_buffers.OutBuf (IO_buffers.OutBufIdx) := B;
    IO_buffers.OutBufIdx := IO_buffers.OutBufIdx + 1;
    if IO_buffers.OutBufIdx > IO_buffers.OutBuf.all'Last then
      Write_Block (IO_buffers, input_size_known, input_size, output, output_size, crypto);
    end if;
  end Put_Byte;

  procedure Flush_Output is
  begin
    if IO_buffers.OutBufIdx > 1 then
      Write_Block (IO_buffers, input_size_known, input_size, output, output_size, crypto);
    end if;
  end Flush_Output;

  feedback_milestone,
  Bytes_in   : Zip_Streams.ZS_Size_Type := 0;   --  Count of input file bytes processed
  user_aborting : Boolean;
  PctDone : Natural;

  function Read_Byte return Byte is
    b : Byte;
    use Zip_Streams;
  begin
    b := IO_buffers.InBuf (IO_buffers.InBufIdx);
    IO_buffers.InBufIdx := IO_buffers.InBufIdx + 1;
    Zip.CRC_Crypto.Update (CRC, (1 => b));
    Bytes_in := Bytes_in + 1;
    if feedback /= null then
      if Bytes_in = 1 then
        feedback (0, False, user_aborting);
      end if;
      if feedback_milestone > 0 and then
         ((Bytes_in - 1) mod feedback_milestone = 0
          or Bytes_in = ZS_Size_Type (input_size))
      then
        if input_size_known then
          PctDone := Integer ((100.0 * Float (Bytes_in)) / Float (input_size));
          feedback (PctDone, False, user_aborting);
        else
          feedback (0, False, user_aborting);
        end if;
        if user_aborting then
          raise User_abort;
        end if;
      end if;
    end if;
    return b;
  end Read_Byte;

  function More_Bytes return Boolean with Inline is
  begin
    if IO_buffers.InBufIdx > IO_buffers.MaxInBufIdx then
      Read_Block (IO_buffers, input);
    end if;
    return not IO_buffers.InputEoF;
  end More_Bytes;

  use BZip2, BZip2.Encoding;

  procedure BZip2_Encode is
    new BZip2.Encoding.Encode (Read_Byte, More_Bytes, Put_Byte);

begin
  Allocate_Buffers (IO_buffers, input_size_known, input_size);
  output_size := 0;
  begin
    Read_Block (IO_buffers, input);
    if input_size_known then
      feedback_milestone := Zip_Streams.ZS_Size_Type (input_size / feedback_steps);
    end if;
    BZip2_Encode
      (case method is
         when BZip2_1 => block_100k,
         when BZip2_2 => block_400k,
         when BZip2_3 => block_900k);
    Flush_Output;
    compression_ok := True;
  exception
    when Compression_inefficient =>
      compression_ok := False;
  end;
  Deallocate_Buffers (IO_buffers);
exception
  when others =>
    Deallocate_Buffers (IO_buffers);
    raise;
end Zip.Compress.BZip2_E;
