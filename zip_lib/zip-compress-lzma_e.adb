--  Legal licensing note:

--  Copyright (c) 2016 .. 2019 Gautier de Montmollin
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

with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

procedure Zip.Compress.LZMA_E
 (input,
  output           : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known : Boolean;
  input_size       : File_size_type;
  feedback         : Feedback_proc;
  method           : LZMA_Method;
  CRC              : in out Interfaces.Unsigned_32; -- only updated here
  crypto           : in out Crypto_pack;
  output_size      : out File_size_type;
  compression_ok   : out Boolean -- indicates compressed < uncompressed
 )
is

  ------------------
  -- Buffered I/O --
  ------------------

  --  Define data types needed to implement input and output file buffers

  procedure Dispose is
    new Ada.Unchecked_Deallocation (Byte_Buffer, p_Byte_Buffer);

  InBuf  : p_Byte_Buffer;  --  I/O buffers
  OutBuf : p_Byte_Buffer;

  InBufIdx  : Positive;  --  Points to next char in buffer to be read
  OutBufIdx : Positive;  --  Points to next free space in output buffer

  MaxInBufIdx : Natural;  --  Count of valid chars in input buffer
  InputEoF : Boolean;     --  End of file indicator

  procedure Read_Block is
  begin
    Zip.Block_Read (
      stream        => input,
      buffer        => InBuf.all,
      actually_read => MaxInBufIdx
    );
    InputEoF := MaxInBufIdx = 0;
    InBufIdx := 1;
  end Read_Block;

  --  Exception for the case where compression works but produces
  --  a bigger file than the file to be compressed (data is too "random").
  Compression_inefficient : exception;

  procedure Write_Block is
    amount : constant Integer := OutBufIdx - 1;
  begin
    output_size := output_size + File_size_type (Integer'Max (0, amount));
    if input_size_known and then output_size >= input_size then
      --  The compression so far is obviously unefficient for that file.
      --  Useless to go further.
      --  Stop immediately before growing the file more than the
      --  uncompressed size.
      raise Compression_inefficient;
    end if;
    Encode (crypto, OutBuf (1 .. amount));
    Zip.Block_Write (output, OutBuf (1 .. amount));
    OutBufIdx := 1;
  end Write_Block;

  procedure Put_byte (B : Unsigned_8) is
  begin
    OutBuf (OutBufIdx) := B;
    OutBufIdx := OutBufIdx + 1;
    if OutBufIdx > OutBuf.all'Last then
      Write_Block;
    end if;
  end Put_byte;

  procedure Flush_output is
  begin
    if OutBufIdx > 1 then
      Write_Block;
    end if;
  end Flush_output;

  X_Percent : Natural;
  Bytes_in  : Natural;   --  Count of input file bytes processed
  user_aborting : Boolean;
  PctDone : Natural;

  function Read_byte return Byte is
    b : Byte;
  begin
    b := InBuf (InBufIdx);
    InBufIdx := InBufIdx + 1;
    Zip.CRC_Crypto.Update (CRC, (1 => b));
    Bytes_in := Bytes_in + 1;
    if feedback /= null then
      if Bytes_in = 1 then
        feedback (0, False, user_aborting);
      end if;
      if X_Percent > 0 and then
         ((Bytes_in - 1) mod X_Percent = 0
          or Bytes_in = Integer (input_size))
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
    if InBufIdx > MaxInBufIdx then
      Read_Block;
    end if;
    return not InputEoF;
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
      LZMA_2_for_Zip_in_Zip => (8, 4, 0, Level_2),
      LZMA_3_for_Zip_in_Zip => (8, 4, 0, Level_3),
      LZMA_2_for_Source     => (3, 0, 0, Level_2),
      LZMA_3_for_Source     => (3, 0, 0, Level_3)
    );

  procedure LZMA_Encode is
    new LZMA.Encoding.Encode (Read_byte, More_bytes, Put_byte);

begin
  --  Allocate input and output buffers.
  if input_size_known then
    InBuf := new Byte_Buffer
      (1 .. Integer'Min (Integer'Max (8, Integer (input_size)), buffer_size));
  else
    InBuf := new Byte_Buffer (1 .. buffer_size);
  end if;
  OutBuf := new Byte_Buffer (1 .. buffer_size);
  OutBufIdx := 1;
  output_size := 0;
  begin
    Read_Block;
    Bytes_in := 0;
    if input_size_known then
      X_Percent := Integer (input_size / 40);
    else
      X_Percent := 0;
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
  Dispose (InBuf);
  Dispose (OutBuf);
end Zip.Compress.LZMA_E;
