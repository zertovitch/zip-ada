-- The "Deflate" method combines the LZ77 compression method with some Hufman
-- encoding gymnastics.
--
-- See package specification for details.
--

-- Change log:
--
-- 17-Feb-2011: Created

with Interfaces; use Interfaces;
with Zip.LZ77, Zip.CRC;
with Zip_Streams;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Zip.Compress.Deflate
 (input,
  output          : Zip_Streams.Zipstream_Class;
  input_size_known: Boolean;
  input_size      : File_size_type;
  feedback        : Feedback_proc;
  method          : Deflation_Method;
  CRC             : in out Interfaces.Unsigned_32; -- only updated here
  output_size     : out File_size_type;
  compression_ok  : out Boolean -- indicates compressed < uncompressed
)
is
  use Zip_Streams;

  ------------------
  -- Buffered I/O --
  ------------------

  --  Define data types needed to implement input and output file buffers

  BUFSIZE: constant  := 10240;

  InBuf, OutBuf: Byte_Buffer(1..BUFSIZE);

  InBufIdx: Positive;  --  Points to next char in buffer to be read
  OutBufIdx: Positive; --  Points to next free space in output buffer

  MaxInBufIdx: Natural;  --  Count of valid chars in input buffer
  InputEoF: Boolean;     --  End of file indicator

  procedure Read_Block is
  begin
    Zip.BlockRead(
      stream        => input,
      buffer        => InBuf,
      actually_read => MaxInBufIdx
    );
    InputEoF:= MaxInBufIdx = 0;
    InBufIdx := 1;
  end Read_Block;

  -- Exception for the case where compression works but produces
  -- a bigger file than the file to be compressed (data is too "random").
  Compression_unefficient: exception;

  procedure Write_Block is
    amount: constant Integer:= OutBufIdx-1;
  begin
    output_size:= output_size + File_size_type(Integer'Max(0,amount));
    if input_size_known and then output_size >= input_size then
      -- The compression so far is obviously unefficient for that file.
      -- Useless to go further.
      -- Stop immediately before growing the file more than the
      -- uncompressed size.
      raise Compression_unefficient;
    end if;
    Zip.BlockWrite(output.all, OutBuf(1 .. amount));
    OutBufIdx := 1;
  end Write_Block;

  procedure Put_byte(B : Byte) is
  begin
    OutBuf(OutBufIdx) := B;
    OutBufIdx:= OutBufIdx + 1;
    if OutBufIdx > OutBuf'Last then
      Write_Block;
    end if;
  end Put_byte;

  --------------------------------------------------------------------------

  -----------------
  -- Code buffer --
  -----------------

  Save_byte: Byte;  --  Output code buffer
  Bits_used: Byte;  --  Index into output code buffer

  procedure Flush_output is
  begin
    if Bits_used /= 0 then
      Put_byte(Save_byte);
    end if;
    if OutBufIdx > 1 then
      Write_Block;
    end if;
  end Flush_output;

  type U32 is mod 2**32;

  procedure Put_code(code: U32; code_size: Natural) is
    code_work: U32:= code;
    temp, Save_byte_local, Bits_used_local: Byte;
  begin
    temp:= 0;
    Save_byte_local:= Save_byte;
    Bits_used_local:= Bits_used;
    for count in reverse 1 .. code_size loop
      temp:= 0;
      if code_work mod 2 = 1 then
        temp:= temp + 1;
      end if;
      code_work:= code_work  / 2;
      temp:= Shift_Left(temp, Integer(Bits_used_local));
      Bits_used_local:= Bits_used_local+1;
      Save_byte_local:= Save_byte_local or temp;
      if Bits_used_local = 8 then
        Put_byte(Save_byte_local);
        Save_byte_local:= 0;
        temp:= 0;
        Bits_used_local:= 0;
      end if;
    end loop;
    Save_byte:= Save_byte_local;
    Bits_used:= Bits_used_local;
  end Put_code;

  --------------------------------
  -- LZ77 front-end compression --
  --------------------------------

  procedure Encode is

    X_Percent: Natural;
    Bytes_in   : Natural;   --  Count of input file bytes processed
    user_aborting: Boolean;
    PctDone: Natural;

    function Read_byte return Byte is
      b: Byte;
    begin
      b:= InBuf(InBufIdx);
      InBufIdx:= InBufIdx + 1;
      Zip.CRC.Update(CRC, (1=> b));
      Bytes_in:= Bytes_in + 1;
      if feedback /= null then
        if Bytes_in = 1 then
          feedback(0, False, user_aborting);
        end if;
        if X_Percent > 0 and then
           ((Bytes_in-1) mod X_Percent = 0
            or Bytes_in = Integer(input_size))
        then
          if input_size_known then
            PctDone := Integer( (100.0 * Float( Bytes_in)) / Float(input_size));
            feedback(PctDone, False, user_aborting);
          else
            feedback(0, False, user_aborting);
          end if;
          if user_aborting then
            raise User_abort;
          end if;
        end if;
      end if;
      return b;
    end Read_byte;

    function More_bytes return Boolean is
    begin
      if InBufIdx > MaxInBufIdx then
        Read_Block;
      end if;
      return not InputEoF;
    end More_bytes;

    -- LZ77 params
    Look_Ahead         : constant Integer:= 31; -- !! find adequate value
    String_buffer_size : constant := 2**12; -- 2**n optimizes "mod" to "and"
    Threshold          : constant := 3;

    -- if the DLE coding doesn't fit the format constraints, we
    -- need to decode it as a simple sequence of litterals

    type Text_Buffer is array ( 0..String_buffer_size+Look_Ahead-1 ) of Byte;
    Text_Buf: Text_Buffer;
    R: Natural;

    ------------------------------------------------------
    -- Deflate, post LZ encoding, with Huffman encoding --
    ------------------------------------------------------

    type Length_code_pair is record
      length: Natural;
      code  : Natural;
    end record;

    procedure Invert(lc: in out Length_code_pair) is
      a: Natural:= lc.code;
      b: Natural:= 0;
    begin
      for i in 1..lc.length loop
        b:= b * 2 + a mod 2;
        a:= a / 2;
      end loop;
      lc.code:= b;
    end Invert;

    type Huff_descriptor is array(Natural range <>) of Length_code_pair;

    procedure Invert(hd: in out Huff_descriptor) is
    begin
      for i in hd'Range loop
        Invert(hd(i));
      end loop;
    end Invert;

    procedure Put_code(lc: Length_code_pair) is
    begin
      Put_code(U32(lc.code), lc.length);
    end Put_code;

    descr_lit_len: Huff_descriptor(0..287);
    -- current tree descriptor for litteral or length encoding
    descr_dis: Huff_descriptor(0..29);

    -------------------------------------------------------
    -- Deflate with fixed (pre-defined) Huffman encoding --
    -------------------------------------------------------

    procedure Prepare_Huffman_codes is
    begin
      if method = deflate_fixed then
        for i in 0 .. 143 loop
          descr_lit_len(i):= (length => 8, code => 16#30#+i);
        end loop;
        for i in 144 .. 255 loop
          descr_lit_len(i):= (length => 9, code => 16#190#+(i-144));
        end loop;
        for i in 256 .. 279 loop
          descr_lit_len(i):= (length => 7, code => i-256);
        end loop;
        for i in 280 .. 287 loop
          descr_lit_len(i):= (length => 8, code => 16#C0#+(i-280));
        end loop;
        for i in 0 .. 29 loop
          descr_dis(i):= (length => 5, code => i);
        end loop;
      else
        raise Program_Error;
        -- only code for fixed block has been done so far !!
      end if;
      -- Invert bit order for output:
      Invert(descr_lit_len);
      Invert(descr_dis);
    end Prepare_Huffman_codes;

    -- Write a normal, "clear-text", character (litteral)
    procedure Write_normal_byte( b: Byte ) is
    begin
      Put_code( descr_lit_len(Integer(b)) );
      -- put("{"&character'val(b)&"}");
      Text_Buf(R):= b;
      R:= (R+1) mod String_buffer_size;
    end Write_normal_byte;

    procedure Write_DL_code( distance, length: Integer ) is
      Copy_start: constant Natural:= (R - distance) mod String_buffer_size;
      -- Possible ranges for distance and length encoding
      -- in the Zip-Deflate format:
      subtype Length_range is Integer range 3 .. 18; -- 258 !!
      subtype Distance_range is Integer range 1 .. 16; -- 32768 !!
    begin
      if distance in Distance_range and length in Length_range then
        -- put('('&distance'img & ',' & length'img&')');

        --                             Length Codes
        --                             ------------
        --      Extra             Extra              Extra              Extra
        -- Code Bits Length  Code Bits Lengths  Code Bits Lengths  Code Bits Length(s)
        -- ---- ---- ------  ---- ---- -------  ---- ---- -------  ---- ---- ---------
        --  257   0     3     265   1   11,12    273   3   35-42    281   5  131-162
        --  258   0     4     266   1   13,14    274   3   43-50    282   5  163-194
        --  259   0     5     267   1   15,16    275   3   51-58    283   5  195-226
        --  260   0     6     268   1   17,18    276   3   59-66    284   5  227-257
        --  261   0     7     269   2   19-22    277   4   67-82    285   0    258
        --  262   0     8     270   2   23-26    278   4   83-98
        --  263   0     9     271   2   27-30    279   4   99-114
        --  264   0    10     272   2   31-34    280   4  115-130
        case Length_range(length) is
          when 3..10 =>
            Put_code( descr_lit_len(254+length) );
            -- Code 257..264, with no extra bit
          when 11..18 =>
            Put_code( descr_lit_len( 265 + (length-11) / 2 ));
            -- Code 265..268 with 1 extra bit
            Put_code( U32((length-11) mod 2), 1);
        end case;
        --                            Distance Codes
        --                            --------------
        --      Extra           Extra             Extra               Extra
        -- Code Bits Dist  Code Bits  Dist   Code Bits Distance  Code Bits Distance
        -- ---- ---- ----  ---- ---- ------  ---- ---- --------  ---- ---- --------
        --   0   0    1      8   3   17-24    16    7  257-384    24   11  4097-6144
        --   1   0    2      9   3   25-32    17    7  385-512    25   11  6145-8192
        --   2   0    3     10   4   33-48    18    8  513-768    26   12  8193-12288
        --   3   0    4     11   4   49-64    19    8  769-1024   27   12 12289-16384
        --   4   1   5,6    12   5   65-96    20    9 1025-1536   28   13 16385-24576
        --   5   1   7,8    13   5   97-128   21    9 1537-2048   29   13 24577-32768
        --   6   2   9-12   14   6  129-192   22   10 2049-3072
        --   7   2  13-16   15   6  193-256   23   10 3073-4096
        case Distance_range(distance) is
          when 1..4 =>
            Put_code( descr_dis(distance-1) );
            -- Code with no extra bit
          when 5..8 =>
            Put_code( descr_dis( 4 + (distance-5) / 2 ));
            -- Code with 1 extra bit
            Put_code( U32((distance-5) mod 2), 1);
          when 9..16 =>
            Put_code( descr_dis( 6 + (distance-9) / 4 ));
            -- Code with 2 extra bits
            Put_code( U32((distance-9) mod 4), 2);
        end case;
        -- Expand in the circular text buffer to have it up to date
        for K in 0..length-1 loop
          Text_Buf(R):= Text_Buf((Copy_start+K) mod String_buffer_size);
          R:= (R+1) mod String_buffer_size;
        end loop;
      else
        -- Cannot encode this distance-length pair, then expand to output :-(
        -- if phase= compress then Put("Aie! (" & distance'img & length'img & ")"); end if;
        for K in 0..length-1 loop
          Write_normal_byte( Text_Buf((Copy_start+K) mod String_buffer_size) );
        end loop;
      end if;
    end Write_DL_code;

    procedure My_LZ77 is
      new LZ77(
        String_buffer_size, Look_Ahead, Threshold,
        Read_byte, More_bytes,
        Write_normal_byte, Write_DL_code
      );

  begin -- Encode
    Prepare_Huffman_codes;
    Read_Block;
    R:= String_buffer_size-Look_Ahead;
    Bytes_in := 0;
    if input_size_known then
      X_Percent := Integer(input_size / 40);
    else
      X_Percent := 0;
    end if;
    -- !! only for 1 fixed block
    Put_code(code => 1, code_size => 1); -- signals last block
    Put_code(code => 1, code_size => 2); -- signals a fixed block
    My_LZ77;
    Put_code(descr_lit_len(256)); -- signals end of block (EOB)
  end Encode;

begin
  OutBufIdx := 1;
  output_size:= 0;
  --  Initialize output bit buffer
  Save_byte := 0;
  Bits_used := 0;
  Encode;
  Flush_output;
  compression_ok:= True;
exception
  when Compression_unefficient =>
    compression_ok:= False;
end Zip.Compress.Deflate;
