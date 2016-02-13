-- The "Deflate" method combines the LZ77 compression method with some Huffman
-- encoding gymnastics.
--
-- See package specification for details.
--
-- To do:
--  - compute cost/benefit of various choices, e.g. DLE encoding (versus string of literals)
--
-- Change log:
--
--  4-Feb-2016: Start of "Dynamic" encoding format (compression structure sent before block)
-- 19-Feb-2011: All distance and length codes implemented.
-- 18-Feb-2011: First version working with Deflate fixed and restricted
--                distance & length codes.
-- 17-Feb-2011: Created.

with Zip.LZ77, Zip.CRC_Crypto;
with Zip_Streams;

with Length_limited_Huffman_code_lengths;

with Ada.Exceptions;                    use Ada.Exceptions;
with Interfaces;                        use Interfaces;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Zip.Compress.Deflate
 (input,
  output          : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known: Boolean;
  input_size      : File_size_type;
  feedback        : Feedback_proc;
  method          : Deflation_Method;
  CRC             : in out Interfaces.Unsigned_32; -- only updated here
  crypto          : in out Crypto_pack;
  output_size     : out File_size_type;
  compression_ok  : out Boolean -- indicates compressed < uncompressed
)
is
  use Zip_Streams;

  ------------------
  -- Buffered I/O --
  ------------------

  --  Define data types needed to implement input and output file buffers

  InBuf, OutBuf: Byte_Buffer(1..buffer_size);

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
    Encode(crypto, OutBuf(1 .. amount));
    Zip.BlockWrite(output, OutBuf(1 .. amount));
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
      Zip.CRC_Crypto.Update(CRC, (1=> b));
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
    Look_Ahead         : constant Integer:= 258;
    String_buffer_size : constant := 2**15; -- 2**n optimizes "mod" to "and"
    Threshold          : constant := 3;

    -- if the DLE coding doesn't fit the format constraints, we
    -- need to decode it as a simple sequence of literals

    type Text_Buffer is array ( 0..String_buffer_size+Look_Ahead-1 ) of Byte;
    Text_Buf: Text_Buffer;
    R: Natural;

    ------------------------------------------------------
    -- Deflate, post LZ encoding, with Huffman encoding --
    ------------------------------------------------------

    invalid: constant:= -1;

    subtype Huffman_code_range is Integer range invalid .. Integer'Last;

    type Length_code_pair is record
      length : Natural;                       --  Huffman code length, in bits
      code   : Huffman_code_range:= invalid;  --  The code itself
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

    type Deflate_Huff_descriptors is record
      --  Tree descriptor for Literal, EOB or Length encoding
      lit_len: Huff_descriptor(0..287);
      --  Tree descriptor for Distance encoding
      dis: Huff_descriptor(0..29);
    end record;

    --  The Huffman tree is completely determined by the bit length to be used for reaching
    --  leaf nodes, thanks to two special rules (See RFC 1951, section 3.2.2).
    --  The Prepare_Huffman_codes function finds the Huffman code for each value, given
    --  the bit length imposed as input.

    max_huffman_bits: constant:= 15;

    procedure Prepare_tree(hd: in out Huff_descriptor) is
      bl_count, next_code: array(0..max_huffman_bits) of Natural:= (others => 0);
      code: Natural:= 0;
      bl: Natural;
    begin
      --  Algorithm from RFC 1951, section 3.2.2.
      --  Step 1)
      for i in hd'Range loop
        bl:= hd(i).length;
        bl_count(bl):= bl_count(bl) + 1;  --  One more code to be defined with bit length bl
      end loop;
      --  Step 2)
      for bits in 1 .. max_huffman_bits loop
        code:= (code + bl_count(bits-1)) * 2;
        next_code(bits):= code;  --  This will be the first code for bit length "bits"
      end loop;
      --  Step 3)
      for n in hd'Range loop
        bl:= hd(n).length;
        if bl > 0 then
          hd(n).code:= next_code(bl);
          next_code(bl):= next_code(bl) + 1;
        else
          hd(n).code:= 0;
        end if;
      end loop;
      --  Invert bit order for output:
      Invert(hd);
    end Prepare_tree;

    function Prepare_Huffman_codes(dhd: Deflate_Huff_descriptors) return Deflate_Huff_descriptors
    is
      dhd_var: Deflate_Huff_descriptors:= dhd;
    begin
      Prepare_tree(dhd_var.lit_len);
      Prepare_tree(dhd_var.dis);
      return dhd_var;
    end Prepare_Huffman_codes;

    ------------------------------
    --  Compression structures  --
    ------------------------------

    type Bit_length_array is array(Natural range <>) of Natural;
    subtype Alphabet_lit_len is Natural range 0..287;
    subtype Bit_length_array_lit_len is Bit_length_array(Alphabet_lit_len);
    subtype Alphabet_dis is Natural range 0..29;
    subtype Bit_length_array_dis is Bit_length_array(Alphabet_dis);

    function Build_descriptor(
      bl_for_lit_len : Bit_length_array_lit_len;
      bl_for_dis     : Bit_length_array_dis
    )
    return Deflate_Huff_descriptors
    is
      new_d: Deflate_Huff_descriptors;
    begin
      for i in bl_for_lit_len'Range loop
        new_d.lit_len(i):= (length => bl_for_lit_len(i), code => invalid);
      end loop;
      for i in bl_for_dis'Range loop
        new_d.dis(i):= (length => bl_for_dis(i), code => invalid);
      end loop;
      return Prepare_Huffman_codes(new_d);
    end Build_descriptor;

    --  Default Huffman trees, for "fixed" blocks as defined in appnote.txt or RFC 1951

    default_lit_len_bl: constant Bit_length_array_lit_len:=
      (  0 .. 143 => 8,  --  For literals ("plain text" bytes)
       144 .. 255 => 9,  --  For more literals ("plain text" bytes)
       256 .. 279 => 7,  --  For EOB (256), then for length codes
       280 .. 287 => 8   --  For more length codes
      );
    default_dis_bl: constant Bit_length_array_dis:= (others => 5);

    Deflate_fixed_descriptors: constant Deflate_Huff_descriptors:=
      Build_descriptor( default_lit_len_bl, default_dis_bl);

    --  Huffman codes tuned for compressing a text like a source code...
    custom_lit_len_bl: constant Bit_length_array_lit_len:=
      ( 0 .. 9 | 11 .. 31 | 33 .. 143 => 8,
        10 | 32  => 7,
        144 .. 255 => 9,
        256 => 8,
        257 .. 278 => 7,
        279 .. 287 => 8
       );

    Deflate_custom_descriptors: constant Deflate_Huff_descriptors:=
      Build_descriptor
        ( bl_for_lit_len => custom_lit_len_bl,
          bl_for_dis =>
            ( 0 => 11, 1 => 9, 2 => 10, 3 => 8, 4 => 7, 5 .. 7 => 6,
              10 | 12 | 14 | 16 | 18 | 20 | 22 | 24 => 4,
              others => 5 )
        );

    --  Emit a Huffman code
    procedure Put_code(lc: Length_code_pair) is
    begin
      if lc.length = 0 then
        Raise_Exception(Constraint_Error'Identity, "Huffman code of length 0 should not occurr");
      end if;
      Put_code(U32(lc.code), lc.length);
    end Put_code;

    procedure Put_compression_structure(dhd: Deflate_Huff_descriptors) is
      subtype Alphabet is Integer range 0..18;
      type Alpha_Array is new Bit_length_array(Alphabet);
      truc_freq, truc_bl: Alpha_Array;
      truc: Huff_descriptor(Alphabet);
      bit_order_for_dynamic_block : constant array (Alphabet) of Natural :=
         ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );
      bl: Natural;
      procedure LLHCL is new
        Length_limited_Huffman_code_lengths(Alphabet, Natural, Alpha_Array, Alpha_Array, 7);
    begin
      Put_code(288 - 257, 5);  --  !! maximum = 288 - 257
      Put_code(30  -   1, 5);  --  !! maximum
      Put_code(19  -   4, 4);  --  !! maximum
      truc_freq:= (others => 1);
      -- ^ !! 1: pretend all bl-codes are used at least once, otherwise it bombs when sending
      -- a bit length that never appears.
      for i in dhd.lit_len'Range loop
        bl:= dhd.lit_len(i).length;
        truc_freq(bl):= truc_freq(bl) + 1;  --  +1 for bl's histogram bar
      end loop;
      for i in dhd.dis'Range loop
        bl:= dhd.dis(i).length;
        truc_freq(bl):= truc_freq(bl) + 1;  --  +1 for bl's histogram bar
      end loop;
      --  We have now statistics of all bit lengths occurrences of both Huffman
      --  trees used for compressing the data.
      --  Now we turn these counts into bit lengths for the local tree
      --  that helps us to store the compression structure in a more compact form.
      LLHCL(truc_freq, truc_bl);  --  Call to the magic algorithm
      for i in Alphabet loop
        truc(i).length:= truc_bl(i);
      end loop;
      Prepare_tree(truc);         --  Build the Huffman codes described by the bit lengths
      for i in Alphabet loop
        Put_code(U32(truc(bit_order_for_dynamic_block(i)).length), 3);
      end loop;
      --  !! Not RLE compressed (no repeat codes)
      --  Emit the Huffman lengths for encoding the data, in an Huffman-encoded fashion.
      --  !! skip lengths that never happen.
      for i in dhd.lit_len'Range loop
        Put_code(truc((dhd.lit_len(i).length)));  --  .length is in 0..15
      end loop;
      for i in dhd.dis'Range loop
        Put_code(truc(dhd.dis(i).length));  --  .length is in 0..15
      end loop;
    end Put_compression_structure;

    --  Current tree descriptors
    descr: Deflate_Huff_descriptors:= Deflate_fixed_descriptors;

    --  Write a normal, "clear-text" (post LZ, pre Huffman), 8-bit character (literal)
    procedure Put_literal_byte( b: Byte ) is
    begin
      Put_code( descr.lit_len(Integer(b)) );
      -- put("{"&character'val(b)&"}");
      Text_Buf(R):= b;
      R:= (R+1) mod String_buffer_size;
    end Put_literal_byte;

    --  Possible ranges for distance and length encoding in the Zip-Deflate format:
    subtype Length_range is Integer range 3 .. 258;
    subtype Distance_range is Integer range 1 .. 32768;

    procedure Put_DL_code( distance, length: Integer ) is
    begin
      -- Already checked: distance in Distance_range and length in Length_range
      --
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
      --
      --  Example: the code # 266 means the LZ length (# of message bytes to be copied)
      --           shall be 13 or 14, depending on the extra bit value.
      --
      case Length_range(length) is
        when 3..10 => -- Codes 257..264, with no extra bit
          Put_code( descr.lit_len( 257 + length-3 ) );
        when 11..18 => -- Codes 265..268, with 1 extra bit
          Put_code( descr.lit_len( 265 + (length-11) / 2 ) );
          Put_code( U32((length-11) mod 2), 1 );
        when 19..34 => -- Codes 269..272, with 2 extra bits
          Put_code( descr.lit_len( 269 + (length-19) / 4 ) );
          Put_code( U32((length-19) mod 4), 2 );
        when 35..66 => -- Codes 273..276, with 3 extra bits
          Put_code( descr.lit_len( 273 + (length-35) / 8 ) );
          Put_code( U32((length-35) mod 8), 3 );
        when 67..130 => -- Codes 277..280, with 4 extra bits
          Put_code( descr.lit_len( 277 + (length-67) / 16 ) );
          Put_code( U32((length-67) mod 16), 4 );
        when 131..257 => -- Codes 281..284, with 5 extra bits
          Put_code( descr.lit_len( 281 + (length-131) / 32 ) );
          Put_code( U32((length-131) mod 32), 5 );
        when 258 => -- Code 285, with no extra bit
          Put_code( descr.lit_len( 285 ) );
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
      --
      --
      --  Example: the code # 10 means the LZ distance (# positions back in the circular
      --           message buffer for starting the copy) shall be 33, plus the value given
      --           by the 4 extra bits (between 0 and 15).
      case Distance_range(distance) is
        when 1..4 => -- Codes 0..3, with no extra bit
          Put_code( descr.dis(distance-1) );
        when 5..8 => -- Codes 4..5, with 1 extra bit
          Put_code( descr.dis( 4 + (distance-5) / 2 ) );
          Put_code( U32((distance-5) mod 2), 1 );
        when 9..16 => -- Codes 6..7, with 2 extra bits
          Put_code( descr.dis( 6 + (distance-9) / 4 ) );
          Put_code( U32((distance-9) mod 4), 2 );
        when 17..32 => -- Codes 8..9, with 3 extra bits
          Put_code( descr.dis( 8 + (distance-17) / 8 ) );
          Put_code( U32((distance-17) mod 8), 3 );
        when 33..64 => -- Codes 10..11, with 4 extra bits
          Put_code( descr.dis( 10 + (distance-33) / 16 ) );
          Put_code( U32((distance-33) mod 16), 4 );
        when 65..128 => -- Codes 12..13, with 5 extra bits
          Put_code( descr.dis( 12 + (distance-65) / 32 ) );
          Put_code( U32((distance-65) mod 32), 5 );
        when 129..256 => -- Codes 14..15, with 6 extra bits
          Put_code( descr.dis( 14 + (distance-129) / 64 ) );
          Put_code( U32((distance-129) mod 64), 6 );
        when 257..512 => -- Codes 16..17, with 7 extra bits
          Put_code( descr.dis( 16 + (distance-257) / 128 ) );
          Put_code( U32((distance-257) mod 128), 7 );
        when 513..1024 => -- Codes 18..19, with 8 extra bits
          Put_code( descr.dis( 18 + (distance-513) / 256 ) );
          Put_code( U32((distance-513) mod 256), 8 );
        when 1025..2048 => -- Codes 20..21, with 9 extra bits
          Put_code( descr.dis( 20 + (distance-1025) / 512 ) );
          Put_code( U32((distance-1025) mod 512), 9 );
        when 2049..4096 => -- Codes 22..23, with 10 extra bits
          Put_code( descr.dis( 22 + (distance-2049) / 1024 ) );
          Put_code( U32((distance-2049) mod 1024), 10 );
        when 4097..8192 => -- Codes 24..25, with 11 extra bits
          Put_code( descr.dis( 24 + (distance-4097) / 2048 ) );
          Put_code( U32((distance-4097) mod 2048), 11 );
        when 8193..16384 => -- Codes 26..27, with 12 extra bits
          Put_code( descr.dis( 26 + (distance-8193) / 4096 ) );
          Put_code( U32((distance-8193) mod 4096), 12 );
        when 16385..32768 => -- Codes 28..29, with 13 extra bits
          Put_code( descr.dis( 28 + (distance-16385) / 8192 ) );
          Put_code( U32((distance-16385) mod 8192), 13 );
      end case;
    end Put_DL_code;

    -----------------
    --  LZ Buffer  --
    -----------------

    --  We buffer the LZ codes (plain, or distance/length) to analyse them
    --  and try to do smart things

    type LZ_atom_kind is (plain_byte, distance_length);
    type LZ_atom is record
      kind        : LZ_atom_kind;
      plain       : Byte;
      lz_distance : Natural;
      lz_length   : Natural;
    end record;

    LZ_buffer_size: constant:= 2**14; -- hardcoded !!
    type LZ_buffer_index_type is mod LZ_buffer_size;
    type LZ_buffer_type is array (LZ_buffer_index_type) of LZ_atom;
    lz_buffer: LZ_buffer_type;
    lz_buffer_index: LZ_buffer_index_type:= 0;
    lz_buffer_full: Boolean:= False;
    --  True: all LZ_buffer_size data before lz_buffer_index (modulo!) are real data

    End_Of_Block: constant:= 256;

    procedure Flush_from_0 is
      function Deflate_code_for_LZ_length(length: Natural) return Natural is
      begin
        case Length_range(length) is
          when 3..10 => -- Codes 257..264, with no extra bit
            return 257 + length-3;
          when 11..18 => -- Codes 265..268, with 1 extra bit
            return 265 + (length-11) / 2;
          when 19..34 => -- Codes 269..272, with 2 extra bits
            return 269 + (length-19) / 4;
          when 35..66 => -- Codes 273..276, with 3 extra bits
            return 273 + (length-35) / 8;
          when 67..130 => -- Codes 277..280, with 4 extra bits
            return 277 + (length-67) / 16;
          when 131..257 => -- Codes 281..284, with 5 extra bits
            return 281 + (length-131) / 32;
          when 258 => -- Code 285, with no extra bit
            return 285;
        end case;
      end Deflate_code_for_LZ_length;
      --
      function Deflate_code_for_LZ_distance(distance: Natural) return Natural is
      begin
        case Distance_range(distance) is
          when 1..4 => -- Codes 0..3, with no extra bit
            return distance-1 ;
          when 5..8 => -- Codes 4..5, with 1 extra bit
            return  4 + (distance-5) / 2  ;
          when 9..16 => -- Codes 6..7, with 2 extra bits
            return  6 + (distance-9) / 4  ;
          when 17..32 => -- Codes 8..9, with 3 extra bits
            return  8 + (distance-17) / 8  ;
          when 33..64 => -- Codes 10..11, with 4 extra bits
            return  10 + (distance-33) / 16  ;
          when 65..128 => -- Codes 12..13, with 5 extra bits
            return  12 + (distance-65) / 32  ;
          when 129..256 => -- Codes 14..15, with 6 extra bits
            return  14 + (distance-129) / 64  ;
          when 257..512 => -- Codes 16..17, with 7 extra bits
            return  16 + (distance-257) / 128  ;
          when 513..1024 => -- Codes 18..19, with 8 extra bits
            return  18 + (distance-513) / 256  ;
          when 1025..2048 => -- Codes 20..21, with 9 extra bits
            return  20 + (distance-1025) / 512  ;
          when 2049..4096 => -- Codes 22..23, with 10 extra bits
            return  22 + (distance-2049) / 1024  ;
          when 4097..8192 => -- Codes 24..25, with 11 extra bits
            return  24 + (distance-4097) / 2048  ;
          when 8193..16384 => -- Codes 26..27, with 12 extra bits
            return  26 + (distance-8193) / 4096  ;
          when 16385..32768 => -- Codes 28..29, with 13 extra bits
            return  28 + (distance-16385) / 8192  ;
        end case;
      end Deflate_code_for_LZ_distance;
      --
      type Count_type is range 0..File_size_type'Last/2-1;
      type Stats_lit_len_type is array(Alphabet_lit_len) of Count_type;
      lit_len: Alphabet_lit_len;
      stats_lit_len: Stats_lit_len_type;
      procedure LLHCL_lit_len is new
        Length_limited_Huffman_code_lengths(
          Alphabet_lit_len,
          Count_type,
          Stats_lit_len_type,
          Bit_length_array_lit_len,
          15);
      type Stats_dis_type is array(Alphabet_dis) of Count_type;
      dis: Alphabet_dis;
      stats_dis: Stats_dis_type;
      procedure LLHCL_dis is new
        Length_limited_Huffman_code_lengths(
          Alphabet_dis,
          Count_type,
          Stats_dis_type,
          Bit_length_array_dis,
          15);
      bl_for_lit_len : Bit_length_array_lit_len;
      bl_for_dis     : Bit_length_array_dis;
      default_stat: constant:= 0; -- Should be 0 (codes that never happen have 0 count)
    begin
      -- !! temporary: 1 dyn block per flush
      if lz_buffer_full then
        Put_code(descr.lit_len(End_Of_Block));
      end if;
      --  put_line("*** New dyn block");
      Put_code(code => 0, code_size => 1); -- signals block is not last
      Put_code(code => 2, code_size => 2); -- signals a dynamic block
      stats_lit_len:= (End_Of_Block => 1, others => default_stat);
      --  ^ End_Of_Block has to happen once, but never appears in the stats...
      stats_dis:= (others => default_stat);
      for i in 0 .. lz_buffer_index-1 loop
        case lz_buffer(i).kind is
          when plain_byte =>
            lit_len:= Alphabet_lit_len(lz_buffer(i).plain);
            stats_lit_len(lit_len):= stats_lit_len(lit_len) + 1;
          when distance_length =>
            lit_len:= Deflate_code_for_LZ_length(lz_buffer(i).lz_length);
            stats_lit_len(lit_len):= stats_lit_len(lit_len) + 1;
            dis:= Deflate_code_for_LZ_distance(lz_buffer(i).lz_distance);
            stats_dis(dis):= stats_dis(dis) + 1;
        end case;
      end loop;
      begin
        LLHCL_lit_len(stats_lit_len, bl_for_lit_len);
      exception
        when others =>
          --  New_Line;
          --  for a in Alphabet_lit_len loop
          --    Put_Line(a'img &
          --      "; code length;" & bl_for_lit_len(a)'img &
          --      "; count;" & stats_lit_len(a)'img
          --    );
          --  end loop;
          bl_for_lit_len:= custom_lit_len_bl;
      end;
      LLHCL_dis(stats_dis, bl_for_dis);
      descr:= Build_descriptor(bl_for_lit_len, bl_for_dis);
      Put_compression_structure(descr);
      --
      lz_buffer_full:= True;
      --
      --  put_line(
      --    "*** Flush_from_0, index=" & lz_buffer_index'img &
      --    "  range: 0 .." & LZ_buffer_index_type'image(lz_buffer_index-1));
      for i in 0 .. lz_buffer_index-1 loop
        case lz_buffer(i).kind is
          when plain_byte =>
            Put_literal_byte(lz_buffer(i).plain);
          when distance_length =>
            Put_DL_code(lz_buffer(i).lz_distance, lz_buffer(i).lz_length);
        end case;
      end loop;
    end Flush_from_0;

    procedure Push(a: LZ_atom) is
    begin
      lz_buffer(lz_buffer_index):= a;
      lz_buffer_index:= lz_buffer_index + 1;
      if lz_buffer_index = 0 then
        Flush_from_0;
      end if;
    end Push;

    procedure Put_or_delay_literal_byte( b: Byte ) is
    begin
      case method is
        when Deflate_Fixed | Deflate_Preset =>
          Put_literal_byte(b);  --  Buffering is not needed in these modes
        when Deflate_Dynamic_1 =>
          Push((plain_byte, b, 0, 0));
      end case;
    end Put_or_delay_literal_byte;

    procedure Put_or_delay_DL_code( distance, length: Integer ) is
    begin
      case method is
        when Deflate_Fixed | Deflate_Preset =>
          Put_DL_code(distance, length);  --  Buffering is not needed in these modes
        when Deflate_Dynamic_1 =>
          Push((distance_length, 0, distance, length));
      end case;
    end Put_or_delay_DL_code;

    procedure LZ77_emits_DL_code( distance, length: Integer ) is
      Copy_start: constant Natural:= (R - distance) mod String_buffer_size;
    begin
      if distance in Distance_range and length in Length_range then
        Put_or_delay_DL_code(distance, length);
        -- Expand in the circular text buffer to have it up to date
        for K in 0..length-1 loop
          Text_Buf(R):= Text_Buf((Copy_start+K) mod String_buffer_size);
          R:= (R+1) mod String_buffer_size;
        end loop;
      else
        -- Cannot encode this distance-length pair, then we have to expand to output :-(
        -- if phase= compress then Put("Aie! (" & distance'img & length'img & ")"); end if;
        for K in 0..length-1 loop
          Put_or_delay_literal_byte( Text_Buf((Copy_start+K) mod String_buffer_size) );
        end loop;
      end if;
    end LZ77_emits_DL_code;

    procedure My_LZ77 is
      new LZ77(
        String_buffer_size, Look_Ahead, Threshold,
        Read_byte, More_bytes,
        Put_or_delay_literal_byte, LZ77_emits_DL_code
      );

  begin  --  Encode
    Read_Block;
    R:= String_buffer_size-Look_Ahead;
    Bytes_in := 0;
    if input_size_known then
      X_Percent := Integer(input_size / 40);
    else
      X_Percent := 0;
    end if;
    case method is
      when Deflate_Fixed =>
        -- We have only one compressed data block,
        -- then it is already the last one.
        Put_code(code => 1, code_size => 1); -- signals last block
        -- Fixed (predefined) compression structure
        Put_code(code => 1, code_size => 2); -- signals a fixed block
        --  No compression structure need to be included in the compressed data.
      when Deflate_Preset =>
        Put_code(code => 1, code_size => 1); -- signals last block
        Put_code(code => 2, code_size => 2); -- signals a dynamic block
        descr:= Deflate_custom_descriptors;
        Put_compression_structure(descr);
      when Deflate_Dynamic_1 =>
        null;  --  No start data sent, all is delayed
    end case;
    ------------------------------------------------
    --  The whole compression is happenning here: --
    ------------------------------------------------
    My_LZ77;
    -- Done. Send the code signalling the end of compressed data block:
    case method is
      when Deflate_Fixed | Deflate_Preset =>
        null;
      when Deflate_Dynamic_1 =>
        if lz_buffer_index = 0 then
          null;  --  Already flushed at latest Push, or empty data
        else
          --  new_line; put_line("last flush");
          Flush_from_0;
        end if;
        Put_code(descr.lit_len(End_Of_Block));
        Put_code(code => 1, code_size => 1); -- signals last block
        Put_code(code => 1, code_size => 2); -- signals a fixed block
        descr:= Deflate_fixed_descriptors;
    end case;
    Put_code(descr.lit_len(End_Of_Block));
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
