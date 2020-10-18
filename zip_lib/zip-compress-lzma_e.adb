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

with LZMA.Encoding;
with Zip.CRC_Crypto;

with Interfaces; use Interfaces;

procedure Zip.Compress.LZMA_E
 (input,
  output           : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known : Boolean;
  input_size       : Zip_32_Data_Size_Type;
  feedback         : Feedback_proc;
  method           : LZMA_Method;
  CRC              : in out Interfaces.Unsigned_32; -- only updated here
  crypto           : in out Crypto_pack;
  output_size      : out Zip_32_Data_Size_Type;
  compression_ok   : out Boolean -- indicates compressed < uncompressed
 )
is

  ------------------
  -- Buffered I/O --
  ------------------

  IO_buffers : IO_Buffers_Type;

  procedure Put_byte (B : Unsigned_8) is
  begin
    IO_buffers.OutBuf (IO_buffers.OutBufIdx) := B;
    IO_buffers.OutBufIdx := IO_buffers.OutBufIdx + 1;
    if IO_buffers.OutBufIdx > IO_buffers.OutBuf.all'Last then
      Write_Block (IO_buffers, input_size_known, input_size, output, output_size, crypto);
    end if;
  end Put_byte;

  procedure Flush_output is
  begin
    if IO_buffers.OutBufIdx > 1 then
      Write_Block (IO_buffers, input_size_known, input_size, output, output_size, crypto);
    end if;
  end Flush_output;

  feedback_milestone,
  Bytes_in   : Zip_Streams.ZS_Size_Type := 0;   --  Count of input file bytes processed
  user_aborting : Boolean;
  PctDone : Natural;

  function Read_byte return Byte is
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
  end Read_byte;

  function More_bytes return Boolean is
  pragma Inline (More_bytes);
  begin
    if IO_buffers.InBufIdx > IO_buffers.MaxInBufIdx then
      Read_Block (IO_buffers, input);
    end if;
    return not IO_buffers.InputEoF;
  end More_bytes;

  use LZMA, LZMA.Encoding;

  type LZMA_param_bundle is record
    lc : Literal_context_bits_range;
    lp : Literal_position_bits_range;
    pb : Position_bits_range;
    lz : Compression_level;
  end record;

  --  Set the LZMA parameters tuned depending on the data type.
  --  Hints by Stephan Busch (Squeeze Chart) - thanks!
  --  Parameters optimality tested with commands like "lzma_enc picture.jpg out -b".

  LZMA_param : constant array (LZMA_Method) of LZMA_param_bundle :=
    (
      --  LZMA with default parameters (3, 0, 2) but various LZ77 levels:
      LZMA_0                => (3, 0, 2, Level_0),
      LZMA_1                => (3, 0, 2, Level_1),
      LZMA_2                => (3, 0, 2, Level_2),
      LZMA_3                => (3, 0, 2, Level_3),
      --  Parameter sets for specific data types:
      LZMA_for_ARW          => (8, 4, 4, Level_2),
      LZMA_for_GIF          => (0, 0, 0, Level_1),
      LZMA_for_JPEG         => (8, 0, 0, Level_2),
      LZMA_for_MP3          => (8, 4, 4, Level_2),
      LZMA_for_MP4          => (8, 4, 4, Level_2),
      LZMA_for_ORF          => (8, 0, 0, Level_0),
      LZMA_for_PGM          => (8, 0, 0, Level_0),
      LZMA_for_PPM          => (4, 0, 0, Level_2),
      LZMA_for_PNG          => (8, 0, 2, Level_2),
      LZMA_for_WAV          => (0, 1, 1, Level_2),
      LZMA_for_AU           => (0, 2, 2, Level_2),
      LZMA_2_for_Zip_in_Zip => (8, 4, 0, Level_2),
      LZMA_3_for_Zip_in_Zip => (8, 4, 0, Level_3),
      LZMA_2_for_Source     => (3, 0, 0, Level_2),
      LZMA_3_for_Source     => (3, 0, 0, Level_3)
    );

  procedure LZMA_Encode is
    new LZMA.Encoding.Encode (Read_byte, More_bytes, Put_byte);

begin
  Allocate_Buffers (IO_buffers, input_size_known, input_size);
  output_size := 0;
  begin
    Read_Block (IO_buffers, input);
    if input_size_known then
      feedback_milestone := Zip_Streams.ZS_Size_Type (input_size / feedback_steps);
    end if;
    Put_byte (16);  --  LZMA SDK major version
    Put_byte (02);  --  LZMA SDK minor version
    Put_byte (5);   --  LZMA properties size low byte
    Put_byte (0);   --  LZMA properties size high byte
    if input_size_known then
      LZMA_Encode (
        level                 => LZMA_param (method).lz,
        literal_context_bits  => LZMA_param (method).lc,
        literal_position_bits => LZMA_param (method).lp,
        position_bits         => LZMA_param (method).pb,
        dictionary_size       => Integer (input_size)
      );
    else
      LZMA_Encode (
        level                 => LZMA_param (method).lz,
        literal_context_bits  => LZMA_param (method).lc,
        literal_position_bits => LZMA_param (method).lp,
        position_bits         => LZMA_param (method).pb
      );
    end if;
    Flush_output;
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
end Zip.Compress.LZMA_E;
