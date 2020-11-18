--  Legal licensing note:

--  Copyright (c) 2009 .. 2020 Gautier de Montmollin
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

-----------------
--  The "Deflate" method combines a LZ77 compression
--  method with some Huffman encoding gymnastics.
--
--  Magic numbers in this procedure are adjusted through experimentation and marked with: *Tuned*
--
--  To do:
--    - Taillaule: try with slider and/or initial lz window not centered
--    - Taillaule: compare slider to random and fixed in addition to initial
--    - Taillaule: try L_sup distance
--    - Taillaule: restrict BL_Vector to short LZ distances (long distances perhaps too random)
--    - Taillaule: check LZ vector norms on literals only, too (consider distances & lengths as noise)
--    - Taillaule: use a corpus of files badly compressed by our Deflate comparatively
--        to other Deflates (e.g. 7Z seems better with databases)
--    - Add DeflOpt to slowest method, or approximate it by tweaking
--        distance and length statistics before computing their Huffman codes, or
--        reinvent it by computing the size of emitted codes and trying slight changes
--        to the codes' bit lengths.
--    - Improve LZ77 compression: see Zip.LZ77 to-do list; check with bypass_LZ77 below
--        and various programs based on LZ77 using the trace >= some and the LZ77 dump
--        in UnZip.Decompress.
--    - Make this procedure standalone & generic like LZMA.Encoding;
--        use it in the Zada project (Zlib replacement)
--
--  Change log:
--------------
--
--  16-Mar-2016: Taillaule algorithm: first version ready for release.
--  20-Feb-2016: (rev.305) Start of smarter techniques for "Dynamic" encoding: Taillaule algorithm
--   4-Feb-2016: Start of "Dynamic" encoding format (compression structure sent before block)
--
--  19-Feb-2011: All distance and length codes implemented.
--  18-Feb-2011: First version working with Deflate fixed and restricted distance & length codes.
--  17-Feb-2011: Created (single-block, "fixed" Huffman encoding).

with LZ77, Zip.CRC_Crypto;
with Zip_Streams;

with Length_limited_Huffman_code_lengths;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;                        use Interfaces;

procedure Zip.Compress.Deflate
 (input,
  output           : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known : Boolean;
  input_size       : Zip_32_Data_Size_Type;
  feedback         : Feedback_proc;
  method           : Deflation_Method;
  CRC              : in out Interfaces.Unsigned_32;  --  only updated here
  crypto           : in out Crypto_pack;
  output_size      : out Zip_32_Data_Size_Type;
  compression_ok   : out Boolean  --  indicates compressed < uncompressed
)
is
  --  Options for testing.
  --  All should be on False for normal use of this procedure.

  deactivate_scanning : constant Boolean := False;  --  Impact analysis of the scanning method
  trace               : constant Boolean := False;  --  Log file with details
  trace_descriptors   : constant Boolean := False;  --  Additional logging of Huffman descriptors

  --  A log file is used when trace = True.
  log         : File_Type;
  log_name    : constant String := "Zip.Compress.Deflate.zcd";  --  A CSV with an unusual extension
  sep         : constant Character := ';';

  -------------------------------------
  -- Buffered I/O - byte granularity --
  -------------------------------------

  IO_buffers : IO_Buffers_Type;

  procedure Put_byte (B : Byte) is  --  Put a byte, at the byte granularity level
  pragma Inline (Put_byte);
  begin
    IO_buffers.OutBuf (IO_buffers.OutBufIdx) := B;
    IO_buffers.OutBufIdx := IO_buffers.OutBufIdx + 1;
    if IO_buffers.OutBufIdx > IO_buffers.OutBuf'Last then
      Write_Block (IO_buffers, input_size_known, input_size, output, output_size, crypto);
    end if;
  end Put_byte;

  procedure Flush_byte_buffer is
  begin
    if IO_buffers.OutBufIdx > 1 then
      Write_Block (IO_buffers, input_size_known, input_size, output, output_size, crypto);
    end if;
  end Flush_byte_buffer;

  ------------------------------------------------------
  --  Bit code buffer, for sending data at bit level  --
  ------------------------------------------------------

  --  Output buffer. Bits are inserted starting at the right (least
  --  significant bits). The width of bit_buffer must be at least 16 bits.
  subtype U32 is Unsigned_32;
  bit_buffer : U32 := 0;
  --  Number of valid bits in bit_buffer.  All bits above the last valid bit are always zero.
  valid_bits : Integer := 0;

  procedure Flush_bit_buffer is
  begin
    while valid_bits > 0 loop
      Put_byte (Byte (bit_buffer and 16#FF#));
      bit_buffer := Shift_Right (bit_buffer, 8);
      valid_bits := Integer'Max (0, valid_bits - 8);
    end loop;
    bit_buffer := 0;
  end Flush_bit_buffer;

  --  Bit codes are at most 15 bits for Huffman codes,
  --  or 13 for explicit codes (distance extra bits).
  subtype Code_size_type is Integer range 1 .. 15;

  --  Send a value on a given number of bits.
  procedure Put_code (code : U32; code_size : Code_size_type) is
  pragma Inline (Put_code);
  begin
    --  Put bits from code at the left of existing ones. They might be shifted away
    --  partially on the left side (or even entirely if valid_bits is already = 32).
    bit_buffer := bit_buffer or Shift_Left (code, valid_bits);
    valid_bits := valid_bits + code_size;
    if valid_bits > 32 then
      --  Flush 32 bits to output as 4 bytes
      Put_byte (Byte (bit_buffer and 16#FF#));
      Put_byte (Byte (Shift_Right (bit_buffer,  8) and 16#FF#));
      Put_byte (Byte (Shift_Right (bit_buffer, 16) and 16#FF#));
      Put_byte (Byte (Shift_Right (bit_buffer, 24) and 16#FF#));
      valid_bits := valid_bits - 32;
      --  Empty buffer and put on it the rest of the code
      bit_buffer := Shift_Right (code, code_size - valid_bits);
    end if;
  end Put_code;

  ------------------------------------------------------
  -- Deflate, post LZ encoding, with Huffman encoding --
  ------------------------------------------------------

  invalid : constant := -1;

  subtype Huffman_code_range is Integer range invalid .. Integer'Last;

  type Length_code_pair is record
    bit_length : Natural;                        --  Huffman code length, in bits
    code       : Huffman_code_range := invalid;  --  The code itself
  end record;

  procedure Invert (lc : in out Length_code_pair) is
  pragma Inline (Invert);
    a : Natural := lc.code;
    b : Natural := 0;
  begin
    for i in 1 .. lc.bit_length loop
      b := b * 2 + a mod 2;
      a := a / 2;
    end loop;
    lc.code := b;
  end Invert;

  --  The Huffman code set (and therefore the Huffman tree) is completely determined by
  --  the bit length to be used for reaching leaf nodes, thanks to two special
  --  rules (explanation in RFC 1951, section 3.2.2).
  --
  --  So basically the process is the following:
  --
  --     (A) Gather statistics (just counts) for the alphabet.
  --     (B) Turn these counts into code lengths, by calling Length_limited_Huffman_code_lengths.
  --     (C) Build Huffman codes (the bits to be sent) with a call to Prepare_Huffman_codes.
  --
  --  In short:
  --
  --     data -> (A) -> stats -> (B) -> Huffman codes' bit lengths -> (C) -> Huffman codes

  type Huff_descriptor is array (Natural range <>) of Length_code_pair;

  type Bit_length_array is array (Natural range <>) of Natural;
  subtype Alphabet_lit_len is Natural range 0 .. 287;
  subtype Bit_length_array_lit_len is Bit_length_array (Alphabet_lit_len);
  subtype Alphabet_dis is Natural range 0 .. 31;
  subtype Bit_length_array_dis is Bit_length_array (Alphabet_dis);

  type Deflate_Huff_descriptors is record
    --  Tree descriptor for Literal, EOB or Length encoding
    lit_len : Huff_descriptor (0 .. 287);
    --  Tree descriptor for Distance encoding
    dis : Huff_descriptor (0 .. 31);
  end record;
  --  NB: Appnote: "Literal codes 286-287 and distance codes 30-31 are never used
  --                  but participate in the Huffman construction."
  --  Setting upper bound to 285 for literals leads to invalid codes, sometimes.

  --  Copy bit length vectors into Deflate Huffman descriptors

  function Build_descriptors (
    bl_for_lit_len : Bit_length_array_lit_len;
    bl_for_dis     : Bit_length_array_dis
  )
  return Deflate_Huff_descriptors
  is
    new_d : Deflate_Huff_descriptors;
  begin
    for i in new_d.lit_len'Range loop
      new_d.lit_len (i) := (bit_length => bl_for_lit_len (i), code => invalid);
      if trace_descriptors and then trace and then Is_Open (log) then
        Put (log, Integer'Image (bl_for_lit_len (i)) & sep);
      end if;
    end loop;
    for i in new_d.dis'Range loop
      new_d.dis (i) := (bit_length => bl_for_dis (i), code => invalid);
      if trace_descriptors and then trace and then Is_Open (log) then
        Put (log, Integer'Image (bl_for_dis (i)) & sep);
      end if;
    end loop;
    if trace_descriptors and then trace and then Is_Open (log) then
      New_Line (log);
    end if;
    return new_d;
  end Build_descriptors;

  type Count_type is range 0 .. Zip_32_Data_Size_Type'Last / 2 - 1;
  type Stats_type is array (Natural range <>) of Count_type;

  --  The following is a translation of Zopfli's OptimizeHuffmanForRle (v. 11-May-2016).
  --  Possible gain: shorten the compression header containing the Huffman trees' bit lengths.
  --  Possible loss: since the stats do not correspond anymore exactly to the data
  --  to be compressed, the Huffman trees might be suboptimal.
  --
  --  Zopfli comment:
  --    Changes the population counts in a way that the consequent Huffman tree
  --    compression, especially its rle-part, will be more likely to compress this data
  --    more efficiently.
  --
  procedure Tweak_for_better_RLE (counts : in out Stats_type) is
    length : Integer := counts'Length;
    stride : Integer;
    symbol, sum, limit, new_count : Count_type;
    good_for_rle : array (counts'Range) of Boolean := (others => False);
  begin
    --  1) We don't want to touch the trailing zeros. We may break the
    --     rules of the format by adding more data in the distance codes.
    loop
      if length = 0 then
        return;
      end if;
      exit when counts (length - 1) /= 0;
      length := length - 1;
    end loop;
    --  Now counts(0..length - 1) does not have trailing zeros.
    --
    --  2) Let's mark all population counts that already can be encoded with an rle code.
    --
    --  Let's not spoil any of the existing good rle codes.
    --  Mark any seq of 0's that is longer than 5 as a good_for_rle.
    --  Mark any seq of non-0's that is longer than 7 as a good_for_rle.
    symbol := counts (0);
    stride := 0;
    for i in 0 .. length loop
      if i = length or else counts (i) /= symbol then
        if (symbol = 0 and then stride >= 5) or else (symbol /= 0 and then stride >= 7) then
          for k in 0 .. stride - 1 loop
            good_for_rle (i - k - 1) := True;
          end loop;
        end if;
        stride := 1;
        if i /= length then
          symbol := counts (i);
        end if;
      else
        stride := stride + 1;
      end if;
    end loop;
    --  3) Let's replace those population counts that lead to more rle codes.
    stride := 0;
    limit := counts (0);
    sum := 0;
    for i in 0 .. length loop
      if i = length or else good_for_rle (i)
          or else (i > 0 and then good_for_rle (i - 1))  --  Added from Brotli, item #1
          --  Heuristic for selecting the stride ranges to collapse.
          or else abs (counts (i) - limit) >= 4
      then
        if stride >= 4 or else (stride >= 3 and then sum = 0) then
          --  The stride must end, collapse what we have, if we have enough (4).
          --  GdM: new_count is the average of counts on the stride's interval, upper-rounded.
          new_count := Count_type'Max (1, (sum + Count_type (stride) / 2) / Count_type (stride));
          if sum = 0 then
            --  Don't make an all zeros stride to be upgraded to ones.
            new_count := 0;
          end if;
          for k in 0 .. stride - 1 loop
            --  We don't want to change value at counts(i),
            --  that is already belonging to the next stride. Thus - 1.
            counts (i - k - 1) := new_count;  --  GdM: Replace histogram value by averaged value.
          end loop;
        end if;
        stride := 0;
        sum := 0;
        if i < length - 3 then
          --  All interesting strides have a count of at least 4, at least when non-zeros.
          --  GdM: limit is the average of next 4 counts, upper-rounded.
          limit := (counts (i) + counts (i + 1) + counts (i + 2) + counts (i + 3) + 2) / 4;
        elsif i < length then
          limit := counts (i);
        else
          limit := 0;
        end if;
      end if;
      stride := stride + 1;
      if i /= length then
        sum := sum + counts (i);
      end if;
    end loop;
  end Tweak_for_better_RLE;

  subtype Stats_lit_len_type is Stats_type (Alphabet_lit_len);
  subtype Stats_dis_type is Stats_type (Alphabet_dis);

  --  Phase (B) : we turn statistics into Huffman bit lengths.
  function Build_descriptors (
    stats_lit_len  : Stats_lit_len_type;
    stats_dis      : Stats_dis_type
  )
  return Deflate_Huff_descriptors
  is
    bl_for_lit_len : Bit_length_array_lit_len;
    bl_for_dis     : Bit_length_array_dis;
    procedure LLHCL_lit_len is new
      Length_limited_Huffman_code_lengths (
        Alphabet_lit_len, Count_type, Stats_lit_len_type, Bit_length_array_lit_len, 15
      );
    procedure LLHCL_dis is new
      Length_limited_Huffman_code_lengths (
        Alphabet_dis, Count_type, Stats_dis_type, Bit_length_array_dis, 15
      );
    stats_dis_copy : Stats_dis_type := stats_dis;
    --
    procedure Patch_statistics_for_buggy_decoders is
      --  Workaround for buggy Info-Zip decoder versions.
      --  See "PatchDistanceCodesForBuggyDecoders" in Zopfli's deflate.c
      --  NB: here, we patch the statistics and not the resulting bit lengths,
      --      to be sure we avoid invalid Huffman code sets in the end.
      --  The decoding bug concerns Zlib v.<= 1.2.1, UnZip v.<= 6.0, WinZip v.<=10.0.
      used : Natural := 0;
    begin
      for i in stats_dis_copy'Range loop
        if stats_dis_copy (i) /= 0 then
          used := used + 1;
        end if;
      end loop;
      case used is
        when 0 =>  --  No distance code used at all (data must be almost random).
          stats_dis_copy (0 .. 1) := (1, 1);
        when 1 =>
          if stats_dis_copy (0) = 0 then
            stats_dis_copy (0) := 1;  --  Now, code 0 and some other code have non-zero counts.
          else
            stats_dis_copy (1) := 1;  --  Now, codes 0 and 1 have non-zero counts.
          end if;
        when others =>
          null;  --  No workaround needed when 2 or more distance codes are defined.
      end case;
    end Patch_statistics_for_buggy_decoders;
  begin
    Patch_statistics_for_buggy_decoders;
    LLHCL_lit_len (stats_lit_len, bl_for_lit_len);  --  Call the magic algorithm for setting
    LLHCL_dis (stats_dis_copy, bl_for_dis);         --    up Huffman lengths of both trees
    return Build_descriptors (bl_for_lit_len, bl_for_dis);
  end Build_descriptors;

  --  Here is one original part in the Taillaule algorithm: use of basic
  --  topology (L1, L2 distances) to check similarities between Huffman code sets.

  --  Bit length vector. Convention: 16 is unused bit length (close to the bit length for the
  --  rarest symbols, 15, and far from the bit length for the most frequent symbols, 1).
  --  Deflate uses 0 for unused.
  subtype BL_code is Integer_M32 range 1 .. 16;
  type BL_vector is array (1 .. 288 + 32) of BL_code;

  function Convert (h : Deflate_Huff_descriptors) return BL_vector is
    bv : BL_vector;
    j : Positive := 1;
  begin
    for i in h.lit_len'Range loop
      if h.lit_len (i).bit_length = 0 then
        bv (j) := 16;
      else
        bv (j) := Integer_M32 (h.lit_len (i).bit_length);
      end if;
      j := j + 1;
    end loop;
    for i in h.dis'Range loop
      if h.dis (i).bit_length = 0 then
        bv (j) := 16;
      else
        bv (j) := Integer_M32 (h.dis (i).bit_length);
      end if;
      j := j + 1;
    end loop;
    return bv;
  end Convert;

  --  L1 or Manhattan distance
  function L1_distance (b1, b2 : BL_vector) return Natural_M32 is
    s : Natural_M32 := 0;
  begin
    for i in b1'Range loop
      s := s + abs (b1 (i) - b2 (i));
    end loop;
    return s;
  end L1_distance;

  --  L1, tweaked
  --
  tweak : constant array (BL_code) of Positive_M32 :=
    --  For the origin of the tweak function, see "za_work.xls", sheet "Deflate".
    --  function f3 = 0.20 f1 [logarithmic] + 0.80 * identity
    --  NB: all values are multiplied by 100 for accuracy.
        (100, 255, 379, 490, 594, 694, 791, 885, 978, 1069, 1159, 1249, 1338, 1426, 1513, 1600);
    --  Neutral is:
    --  (100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600)

  --
  function L1_tweaked (b1, b2 : BL_vector) return Natural_M32 is
    s : Natural_M32 := 0;
  begin
    for i in b1'Range loop
      s := s + abs (tweak (b1 (i)) - tweak (b2 (i)));
    end loop;
    return s;
  end L1_tweaked;

  --  L2 or Euclidean distance
  function L2_distance_square (b1, b2 : BL_vector) return Natural_M32 is
    s : Natural_M32 := 0;
  begin
    for i in b1'Range loop
      s := s + (b1 (i) - b2 (i)) ** 2;
    end loop;
    return s;
  end L2_distance_square;

  --  L2, tweaked
  function L2_tweaked_square (b1, b2 : BL_vector) return Natural_M32 is
    s : Natural_M32 := 0;
  begin
    for i in b1'Range loop
      s := s + (tweak (b1 (i)) - tweak (b2 (i))) ** 2;
    end loop;
    return s;
  end L2_tweaked_square;

  type Distance_type is (L1, L1_tweaked, L2, L2_tweaked);

  function Similar (
    h1, h2    : Deflate_Huff_descriptors;
    dist_kind : Distance_type;
    threshold : Natural;
    comment   : String
  )
  return Boolean is
    dist  : Natural_M32;
    thres : Natural_M32 := Natural_M32 (threshold);
  begin
    case dist_kind is
      when L1 =>
        dist := L1_distance (Convert (h1), Convert (h2));
      when L1_tweaked =>
        thres := thres * tweak (1);
        dist  := L1_tweaked (Convert (h1), Convert (h2));
      when L2 =>
        thres := thres * thres;
        dist  := L2_distance_square (Convert (h1), Convert (h2));
      when L2_tweaked =>
        thres := (thres * thres) * (tweak (1) * tweak (1));
        dist  := L2_tweaked_square (Convert (h1), Convert (h2));
    end case;
    if trace then
      Put_Line (log,
        "Checking similarity." & sep &
        Distance_type'Image (dist_kind) & sep &
        "Distance (ev. x100, **2):" & sep & Integer_M32'Image (dist) & sep & sep &
        "Threshold (ev. x100, **2):" & sep & Integer_M32'Image (thres) & sep & sep &
        comment
      );
    end if;
    return dist < thres;
  end Similar;

  --  Another original part in the Taillaule algorithm: the possibility of recycling
  --  Huffman codes. It is possible only if previous block was not stored and if
  --  the new block's used alphabets are included in the old block's used alphabets.
  function Recyclable (h_old, h_new : Deflate_Huff_descriptors) return Boolean is
  begin
    for i in h_old.lit_len'Range loop
      if h_old.lit_len (i).bit_length = 0 and h_new.lit_len (i).bit_length > 0 then
        return False;  --  Code used in new, but not in old
      end if;
    end loop;
    for i in h_old.dis'Range loop
      if h_old.dis (i).bit_length = 0 and h_new.dis (i).bit_length > 0 then
        return False;  --  Code used in new, but not in old
      end if;
    end loop;
    return True;
  end Recyclable;

  --  Phase (C): the Prepare_Huffman_codes procedure finds the Huffman code for each
  --  value, given the bit length imposed as input.
  procedure Prepare_Huffman_codes (hd : in out Huff_descriptor) is
    max_huffman_bits : constant := 15;
    bl_count, next_code : array (0 .. max_huffman_bits) of Natural := (others => 0);
    code : Natural := 0;
    bl : Natural;
  begin
    --  Algorithm from RFC 1951, section 3.2.2.
    --  Step 1)
    for i in hd'Range loop
      bl := hd (i).bit_length;
      bl_count (bl) := bl_count (bl) + 1;  --  One more code to be defined with bit length bl
    end loop;
    --  Step 2)
    for bits in 1 .. max_huffman_bits loop
      code := (code + bl_count (bits - 1)) * 2;
      next_code (bits) := code;  --  This will be the first code for bit length "bits"
    end loop;
    --  Step 3)
    for n in hd'Range loop
      bl := hd (n).bit_length;
      if bl > 0 then
        hd (n).code := next_code (bl);
        next_code (bl) := next_code (bl) + 1;
      else
        hd (n).code := 0;
      end if;
    end loop;
    --  Invert bit order for output:
    for i in hd'Range loop
      Invert (hd (i));
    end loop;
  end Prepare_Huffman_codes;

  --  This is the phase (C) for the pair of alphabets used in the Deflate format.
  function Prepare_Huffman_codes (dhd : Deflate_Huff_descriptors) return Deflate_Huff_descriptors
  is
    dhd_var : Deflate_Huff_descriptors := dhd;
  begin
    Prepare_Huffman_codes (dhd_var.lit_len);
    Prepare_Huffman_codes (dhd_var.dis);
    return dhd_var;
  end Prepare_Huffman_codes;

  --  Emit a variable length Huffman code
  procedure Put_Huffman_code (lc : Length_code_pair) is
  pragma Inline (Put_Huffman_code);
  begin
    --  Huffman code of length 0 should never occur: when constructing
    --  the code lengths (LLHCL) any single occurrence in the statistics
    --  will trigger the build of a code length of 1 or more.
    Put_code (
      code      => U32 (lc.code),
      code_size => Code_size_type (lc.bit_length)  --  Range check for length 0 (if enabled).
    );
  end Put_Huffman_code;

  --  This is where the "dynamic" Huffman trees are sent before the block's data are sent.
  --
  --  The decoder needs to know in advance the pair of trees (1st tree for literals-eob-LZ
  --  lengths, 2nd tree for LZ distances) for decoding the compressed data.
  --  But this information takes some room. Fortunately Deflate allows for compressing it
  --  with a combination of Huffman and Run-Length Encoding (RLE) to make this header smaller.
  --  Concretely, the trees are described by the bit length of each symbol, so the header's
  --  content is a vector of length max 320, whose contents are in the 0 .. 18 range and typically
  --  look like:  ... 8, 8, 9, 7, 8, 10, 6, 8, 8, 8, 8, 8, 11, 8, 9, 8, ...
  --  Clearly this vector has redundancies and can be sent in a compressed form.
  --  In this example, the RLE will compress the string of 8's with a single code 8, then a code 17
  --  (repeat x times). Anyway, the very frequent 8's will be encoded with a small number of
  --  bits (less than the 5 plain bits, or maximum 7 Huffman-encoded bits
  --  needed for encoding integers in the 0 .. 18 range).
  --
  procedure Put_compression_structure (
    dhd           :        Deflate_Huff_descriptors;
    cost_analysis :        Boolean;    --  If True: just simulate the whole, and count needed bits
    bits          : in out Count_type  --  This is incremented when cost_analysis = True
  )
  is
    subtype Alphabet is Integer range 0 .. 18;
    type Alpha_Array is new Bit_length_array (Alphabet);
    truc_freq, truc_bl : Alpha_Array;
    truc : Huff_descriptor (Alphabet);
    --  Compression structure: cs_bl is the "big" array with all bit lengths
    --  for compressing data. cs_bl will be sent compressed, too.
    cs_bl : array (1 .. dhd.lit_len'Length + dhd.dis'Length) of Natural;
    last_cs_bl : Natural;
    max_used_lln_code : Alphabet_lit_len := 0;
    max_used_dis_code : Alphabet_dis     := 0;
    --
    procedure Concatenate_all_bit_lengths is
      idx : Natural := 0;
    begin
      for a in reverse Alphabet_lit_len loop
        if dhd.lit_len (a).bit_length > 0 then
          max_used_lln_code := a;
          exit;
        end if;
      end loop;
      for a in reverse Alphabet_dis loop
        if dhd.dis (a).bit_length > 0 then
          max_used_dis_code := a;
          exit;
        end if;
      end loop;
      --  Copy bit lengths for both trees into one array, cs_bl.
      for a in 0 .. max_used_lln_code loop
        idx := idx + 1;
        cs_bl (idx) := dhd.lit_len (a).bit_length;
      end loop;
      for a in 0 .. max_used_dis_code loop
        idx := idx + 1;
        cs_bl (idx) := dhd.dis (a).bit_length;
      end loop;
      last_cs_bl := idx;
    end Concatenate_all_bit_lengths;
    --
    extra_bits_needed : constant array (Alphabet) of Natural :=
       (16 => 2, 17 => 3, 18 => 7, others => 0);
    --
    type Emission_mode is (simulate, effective);
    --
    procedure Emit_data_compression_structures (emit_mode : Emission_mode) is
      procedure Emit_data_compression_atom (x : Alphabet; extra_code : U32 := 0) is
        --  x is a bit length (value in 0..15), or a RLE instruction
      begin
        case emit_mode is
          when simulate =>
            truc_freq (x) := truc_freq (x) + 1;  --  +1 for x's histogram bar
          when effective =>
            Put_Huffman_code (truc (x));
            declare
              extra_bits : constant Natural := extra_bits_needed (x);
            begin
              if extra_bits > 0 then
                Put_code (extra_code, extra_bits);
              end if;
            end;
        end case;
      end Emit_data_compression_atom;
      idx : Natural := 0;
      rep : Positive;  --  Number of times current atom is repeated, >= 1
    begin
      --  Emit the bit lengths, with some RLE encoding (Appnote: 5.5.3; RFC 1951: 3.2.7)
      idx := 1;
      loop
        rep := 1;  --  Current atom, cs_bl(idx), is repeated 1x so far - obvious, isn't it ?
        for j in idx + 1 .. last_cs_bl loop
          exit when cs_bl (j) /= cs_bl (idx);
          rep := rep + 1;
        end loop;
        --  Now rep is the number of repetitions of current atom, including itself.
        if idx > 1 and then cs_bl (idx) = cs_bl (idx - 1) and then rep >= 3
            --  Better repeat a long sequence of zeros by using codes 17 or 18
            --  just after a 138-long previous sequence:
          and then not (cs_bl (idx) = 0 and then rep > 6)
        then
          rep := Integer'Min (rep, 6);
          Emit_data_compression_atom (16, U32 (rep - 3));    -- 16: "Repeat previous 3 to 6 times"
          idx := idx + rep;
        elsif cs_bl (idx) = 0 and then rep >= 3 then
          --  The 0 bit length may occur on long ranges of an alphabet (unused symbols)
          if rep <= 10 then
            Emit_data_compression_atom (17, U32 (rep - 3));  -- 17: "Repeat zero 3 to 10 times"
          else
            rep := Integer'Min (rep, 138);
            Emit_data_compression_atom (18, U32 (rep - 11)); -- 18: "Repeat zero 11 to 138 times"
          end if;
          idx := idx + rep;
        else
          Emit_data_compression_atom (cs_bl (idx));
          idx := idx + 1;
        end if;
        exit when idx > last_cs_bl;
      end loop;
    end Emit_data_compression_structures;
    --  Alphabet permutation for shortening in-use alphabet.
    --  After the RLE codes 16, 17, 18 and the bit length 0, which are assumed to be always used,
    --  the most usual bit lengths (around 8, which is the "neutral" bit length) appear first.
    --  For example, if the rare bit lengths 1 and 15 don't occur in any of the two Huffman trees
    --  for LZ data, then codes 1 and 15 have a length 0 in the local Alphabet and we can omit
    --  sending the last two bit lengths.
    alphabet_permutation : constant array (Alphabet) of Natural :=
       (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);
    procedure LLHCL is new
      Length_limited_Huffman_code_lengths (Alphabet, Natural, Alpha_Array, Alpha_Array, 7);
    a_non_zero : Alphabet;
  begin
    Concatenate_all_bit_lengths;
    truc_freq := (others => 0);
    Emit_data_compression_structures (simulate);
    --  We have now statistics of all bit lengths occurrences of both Huffman
    --  trees used for compressing the data.
    --  We turn these counts into bit lengths for the local tree
    --  that helps us to store the compression structure in a more compact form.
    LLHCL (truc_freq, truc_bl);  --  Call the magic algorithm for setting up Huffman lengths
    --  At least lengths for codes 16, 17, 18, 0 will always be sent,
    --  even if all other bit lengths are 0 because codes 1 to 15 are unused.
    a_non_zero := 3;
    for a in Alphabet loop
      if a > a_non_zero and then truc_bl (alphabet_permutation (a)) > 0 then
        a_non_zero := a;
      end if;
    end loop;
    if cost_analysis then
      --  In this mode, no data output: we sum up the exact
      --  number of bits needed by the compression header.
      bits := bits + 14 + Count_type (1 + a_non_zero) * 3;
      for a in Alphabet loop
        bits := bits + Count_type (truc_freq (a) * (truc_bl (a) + extra_bits_needed (a)));
      end loop;
    else
      --  We output the compression header to the output stream.
      for a in Alphabet loop
        truc (a).bit_length := truc_bl (a);
      end loop;
      Prepare_Huffman_codes (truc);
      --  Output of the compression structure
      Put_code (U32 (max_used_lln_code - 256), 5);  --  max_used_lln_code is always >= 256 = EOB code
      Put_code (U32 (max_used_dis_code), 5);
      Put_code (U32 (a_non_zero - 3), 4);
      --  Save the local alphabet's Huffman lengths. It's the compression structure
      --  for compressing the data compression structure. Easy, isn't it ?
      for a in 0 .. a_non_zero loop
        Put_code (U32 (truc (alphabet_permutation (a)).bit_length), 3);
      end loop;
      --  Emit the Huffman lengths for encoding the data, in the local Huffman-encoded fashion.
      Emit_data_compression_structures (effective);
    end if;
  end Put_compression_structure;

  End_Of_Block : constant := 256;

  --  Default Huffman trees, for "fixed" blocks, as defined in appnote.txt or RFC 1951
  default_lit_len_bl : constant Bit_length_array_lit_len :=
      (0 .. 143   => 8,  --  For literals ("plain text" bytes)
     144 .. 255   => 9,  --  For more literals ("plain text" bytes)
     End_Of_Block => 7,  --  For EOB (256)
     257 .. 279   => 7,  --  For length codes
     280 .. 287   => 8   --  For more length codes
    );
  default_dis_bl : constant Bit_length_array_dis := (others => 5);

  Deflate_fixed_descriptors : constant Deflate_Huff_descriptors :=
    Prepare_Huffman_codes (Build_descriptors (default_lit_len_bl, default_dis_bl));

  --  Current tree descriptors
  curr_descr : Deflate_Huff_descriptors := Deflate_fixed_descriptors;

  --  Write a normal, "clear-text" (post LZ, pre Huffman), 8-bit character (literal)
  procedure Put_literal_byte (b : Byte) is
  begin
    Put_Huffman_code (curr_descr.lit_len (Integer (b)));
  end Put_literal_byte;

  --  Possible ranges for distance and length encoding in the Zip-Deflate format:
  subtype Length_range is Integer range 3 .. 258;
  subtype Distance_range is Integer range 1 .. 32768;

  --  This is where LZ distance-length tokens are written to the output stream.
  --  The Deflate format defines a sort of logarithmic compression, with codes
  --  for various distance and length ranges, plus extra bits for specifying the
  --  exact values. The codes are sent as Huffman codes with variable bit lengths
  --  (nothing to do with the lengths of LZ distance-length tokens).

  --                              Length Codes
  --                              ------------
  --       Extra             Extra              Extra              Extra
  --  Code Bits Length  Code Bits Lengths  Code Bits Lengths  Code Bits Length(s)
  --  ---- ---- ------  ---- ---- -------  ---- ---- -------  ---- ---- ---------
  --   257   0     3     265   1   11,12    273   3   35-42    281   5  131-162
  --   258   0     4     266   1   13,14    274   3   43-50    282   5  163-194
  --   259   0     5     267   1   15,16    275   3   51-58    283   5  195-226
  --   260   0     6     268   1   17,18    276   3   59-66    284   5  227-257
  --   261   0     7     269   2   19-22    277   4   67-82    285   0    258
  --   262   0     8     270   2   23-26    278   4   83-98
  --   263   0     9     271   2   27-30    279   4   99-114
  --   264   0    10     272   2   31-34    280   4  115-130
  --
  --  Example: the code # 266 means the LZ length (# of message bytes to be copied)
  --           shall be 13 or 14, depending on the extra bit value.

  deflate_code_for_lz_length : constant array (Length_range) of Natural :=
      (3          => 257,  -- Codes 257..264, with no extra bit
       4          => 258,
       5          => 259,
       6          => 260,
       7          => 261,
       8          => 262,
       9          => 263,
       10         => 264,
       11  .. 12  => 265,  -- Codes 265..268, with 1 extra bit
       13  .. 14  => 266,
       15  .. 16  => 267,
       17  .. 18  => 268,
       19  .. 22  => 269,  -- Codes 269..272, with 2 extra bits
       23  .. 26  => 270,
       27  .. 30  => 271,
       31  .. 34  => 272,
       35  .. 42  => 273,  -- Codes 273..276, with 3 extra bits
       43  .. 50  => 274,
       51  .. 58  => 275,
       59  .. 66  => 276,
       67  .. 82  => 277,  -- Codes 277..280, with 4 extra bits
       83  .. 98  => 278,
       99  .. 114 => 279,
       115 .. 130 => 280,
       131 .. 162 => 281,  -- Codes 281..284, with 5 extra bits
       163 .. 194 => 282,
       195 .. 226 => 283,
       227 .. 257 => 284,
       258        => 285   -- Code 285, with no extra bit
     );

  extra_bits_for_lz_length_offset : constant array (Length_range) of Integer :=
      (3 ..  10 | 258 =>  invalid,  --  just a placeholder, there is no extra bit there!
      11 ..  18       =>  11,
      19 ..  34       =>  19,
      35 ..  66       =>  35,
      67 .. 130       =>  67,
     131 .. 257       => 131);

  extra_bits_for_lz_length : constant array (Length_range) of Natural :=
      (3 ..  10 | 258 => 0,
      11 ..  18       => 1,
      19 ..  34       => 2,
      35 ..  66       => 3,
      67 .. 130       => 4,
     131 .. 257       => 5);

  procedure Put_DL_code (distance : Distance_range; length : Length_range) is
    extra_bits : Natural;
  begin
    Put_Huffman_code (curr_descr.lit_len (deflate_code_for_lz_length (length)));
    --  Extra bits are needed to differentiate lengths sharing the same code.
    extra_bits := extra_bits_for_lz_length (length);
    if extra_bits > 0 then
      --  We keep only the last extra_bits bits of the length (minus given offset).
      --  Example: if extra_bits = 1, only the parity is sent (0 or 1);
      --  the rest has been already sent with Put_Huffman_code above.
      --  Equivalent: x:= x mod (2 ** extra_bits);
      Put_code (
        U32 (length - extra_bits_for_lz_length_offset (length))
          and
        (Shift_Left (U32'(1), extra_bits) - 1),
        extra_bits);
    end if;
    --                             Distance Codes
    --                             --------------
    --       Extra           Extra             Extra               Extra
    --  Code Bits Dist  Code Bits  Dist   Code Bits Distance  Code Bits Distance
    --  ---- ---- ----  ---- ---- ------  ---- ---- --------  ---- ---- --------
    --    0   0    1      8   3   17-24    16    7  257-384    24   11  4097-6144
    --    1   0    2      9   3   25-32    17    7  385-512    25   11  6145-8192
    --    2   0    3     10   4   33-48    18    8  513-768    26   12  8193-12288
    --    3   0    4     11   4   49-64    19    8  769-1024   27   12 12289-16384
    --    4   1   5,6    12   5   65-96    20    9 1025-1536   28   13 16385-24576
    --    5   1   7,8    13   5   97-128   21    9 1537-2048   29   13 24577-32768
    --    6   2   9-12   14   6  129-192   22   10 2049-3072
    --    7   2  13-16   15   6  193-256   23   10 3073-4096
    --
    --
    --  Example: the code # 10 means the LZ distance (# positions back in the circular
    --           message buffer for starting the copy) shall be 33, plus the value given
    --           by the 4 extra bits (between 0 and 15).
    --
    case distance is
      when 1 .. 4 =>          --  Codes 0..3, with no extra bit
        Put_Huffman_code (curr_descr.dis (distance - 1));
      when 5 .. 8 =>          --  Codes 4..5, with 1 extra bit
        Put_Huffman_code (curr_descr.dis (4 + (distance - 5) / 2));
        Put_code (U32 ((distance - 5) mod 2), 1);
      when 9 .. 16 =>         --  Codes 6..7, with 2 extra bits
        Put_Huffman_code (curr_descr.dis (6 + (distance - 9) / 4));
        Put_code (U32 ((distance - 9) mod 4), 2);
      when 17 .. 32 =>        --  Codes 8..9, with 3 extra bits
        Put_Huffman_code (curr_descr.dis (8 + (distance - 17) / 8));
        Put_code (U32 ((distance - 17) mod 8), 3);
      when 33 .. 64 =>        --  Codes 10..11, with 4 extra bits
        Put_Huffman_code (curr_descr.dis (10 + (distance - 33) / 16));
        Put_code (U32 ((distance - 33) mod 16), 4);
      when 65 .. 128 =>       --  Codes 12..13, with 5 extra bits
        Put_Huffman_code (curr_descr.dis (12 + (distance - 65) / 32));
        Put_code (U32 ((distance - 65) mod 32), 5);
      when 129 .. 256 =>      --  Codes 14..15, with 6 extra bits
        Put_Huffman_code (curr_descr.dis (14 + (distance - 129) / 64));
        Put_code (U32 ((distance - 129) mod 64), 6);
      when 257 .. 512 =>      --  Codes 16..17, with 7 extra bits
        Put_Huffman_code (curr_descr.dis (16 + (distance - 257) / 128));
        Put_code (U32 ((distance - 257) mod 128), 7);
      when 513 .. 1024 =>     --  Codes 18..19, with 8 extra bits
        Put_Huffman_code (curr_descr.dis (18 + (distance - 513) / 256));
        Put_code (U32 ((distance - 513) mod 256), 8);
      when 1025 .. 2048 =>    --  Codes 20..21, with 9 extra bits
        Put_Huffman_code (curr_descr.dis (20 + (distance - 1025) / 512));
        Put_code (U32 ((distance - 1025) mod 512), 9);
      when 2049 .. 4096 =>    --  Codes 22..23, with 10 extra bits
        Put_Huffman_code (curr_descr.dis (22 + (distance - 2049) / 1024));
        Put_code (U32 ((distance - 2049) mod 1024), 10);
      when 4097 .. 8192 =>    --  Codes 24..25, with 11 extra bits
        Put_Huffman_code (curr_descr.dis (24 + (distance - 4097) / 2048));
        Put_code (U32 ((distance - 4097) mod 2048), 11);
      when 8193 .. 16384 =>   --  Codes 26..27, with 12 extra bits
        Put_Huffman_code (curr_descr.dis (26 + (distance - 8193) / 4096));
        Put_code (U32 ((distance - 8193) mod 4096), 12);
      when 16385 .. 32768 =>  --  Codes 28..29, with 13 extra bits
        Put_Huffman_code (curr_descr.dis (28 + (distance - 16385) / 8192));
        Put_code (U32 ((distance - 16385) mod 8192), 13);
    end case;
  end Put_DL_code;

  function Deflate_code_for_LZ_distance (distance : Distance_range) return Natural is
  begin
    case distance is
      when 1 .. 4 => -- Codes 0..3, with no extra bit
        return distance - 1;
      when 5 .. 8 => -- Codes 4..5, with 1 extra bit
        return  4 + (distance - 5) / 2;
      when 9 .. 16 => -- Codes 6..7, with 2 extra bits
        return  6 + (distance - 9) / 4;
      when 17 .. 32 => -- Codes 8..9, with 3 extra bits
        return  8 + (distance - 17) / 8;
      when 33 .. 64 => -- Codes 10..11, with 4 extra bits
        return  10 + (distance - 33) / 16;
      when 65 .. 128 => -- Codes 12..13, with 5 extra bits
        return  12 + (distance - 65) / 32;
      when 129 .. 256 => -- Codes 14..15, with 6 extra bits
        return  14 + (distance - 129) / 64;
      when 257 .. 512 => -- Codes 16..17, with 7 extra bits
        return  16 + (distance - 257) / 128;
      when 513 .. 1024 => -- Codes 18..19, with 8 extra bits
        return  18 + (distance - 513) / 256;
      when 1025 .. 2048 => -- Codes 20..21, with 9 extra bits
        return  20 + (distance - 1025) / 512;
      when 2049 .. 4096 => -- Codes 22..23, with 10 extra bits
        return  22 + (distance - 2049) / 1024;
      when 4097 .. 8192 => -- Codes 24..25, with 11 extra bits
        return  24 + (distance - 4097) / 2048;
      when 8193 .. 16384 => -- Codes 26..27, with 12 extra bits
        return  26 + (distance - 8193) / 4096;
      when 16385 .. 32768 => -- Codes 28..29, with 13 extra bits
        return  28 + (distance - 16385) / 8192;
    end case;
  end Deflate_code_for_LZ_distance;

  -----------------
  --  LZ Buffer  --
  -----------------

  --  We buffer the LZ codes (plain, or distance/length) in order to
  --  analyse them and try to do smart things.

  max_expand : constant := 14;  --  *Tuned* Sometimes it is better to store data and expand short strings
  code_for_max_expand : constant := 266;
  subtype Expanded_data is Byte_Buffer (1 .. max_expand);

  type LZ_atom_kind is (plain_byte, distance_length);
  type LZ_atom is record
    kind          : LZ_atom_kind;
    plain         : Byte;
    lz_distance   : Natural;
    lz_length     : Natural;
    lz_expanded   : Expanded_data;
  end record;

  --  *Tuned*. Min: 2**14, = 16384 (min half buffer 8192)
  --  Optimal so far: 2**17
  LZ_buffer_size : constant := 2**17;
  type LZ_buffer_index_type is mod LZ_buffer_size;
  type LZ_buffer_type is array (LZ_buffer_index_type range <>) of LZ_atom;

  empty_lit_len_stat : constant Stats_lit_len_type := (End_Of_Block => 1, others => 0);
  --  End_Of_Block will have to happen once, but never appears in the LZ statistics...
  empty_dis_stat : constant Stats_dis_type := (others => 0);

  --
  --  Compute statistics for both Literal-length, and Distance alphabets, from a LZ buffer
  --
  procedure Get_statistics (
    lzb           :  in LZ_buffer_type;
    stats_lit_len : out Stats_lit_len_type;
    stats_dis     : out Stats_dis_type
  )
  is
    lit_len : Alphabet_lit_len;
    dis     : Alphabet_dis;
  begin
    stats_lit_len := empty_lit_len_stat;
    stats_dis     := empty_dis_stat;
    for i in lzb'Range loop
      case lzb (i).kind is
        when plain_byte =>
          lit_len := Alphabet_lit_len (lzb (i).plain);
          stats_lit_len (lit_len) := stats_lit_len (lit_len) + 1;     --  +1 for this literal
        when distance_length =>
          lit_len := deflate_code_for_lz_length (lzb (i).lz_length);
          stats_lit_len (lit_len) := stats_lit_len (lit_len) + 1;     --  +1 for this length code
          dis := Deflate_code_for_LZ_distance (lzb (i).lz_distance);
          stats_dis (dis) := stats_dis (dis) + 1;                     --  +1 for this distance code
      end case;
    end loop;
  end Get_statistics;

  --
  --  Send a LZ buffer using currently defined Huffman codes
  --
  procedure Put_LZ_buffer (lzb : LZ_buffer_type) is
  begin
    for i in lzb'Range loop
      case lzb (i).kind is
        when plain_byte =>
          Put_literal_byte (lzb (i).plain);
        when distance_length =>
          Put_DL_code (lzb (i).lz_distance, lzb (i).lz_length);
      end case;
    end loop;
  end Put_LZ_buffer;

  block_to_finish : Boolean := False;
  last_block_marked : Boolean := False;
  type Block_type is (stored, fixed, dynamic, reserved);  --  Appnote, 5.5.2
  --  If last_block_type = dynamic, we may recycle previous block's Huffman codes
  last_block_type : Block_type := reserved;

  procedure Mark_new_block (last_block_for_stream : Boolean) is
  begin
    if block_to_finish and last_block_type in fixed .. dynamic then
      Put_Huffman_code (curr_descr.lit_len (End_Of_Block));  --  Finish previous block
    end if;
    block_to_finish := True;
    Put_code (code => Boolean'Pos (last_block_for_stream), code_size => 1);
    last_block_marked := last_block_for_stream;
  end Mark_new_block;

  --  Send a LZ buffer completely decoded as literals (LZ compression is discarded)
  procedure Expand_LZ_buffer (lzb : LZ_buffer_type; last_block : Boolean) is
    b1, b2 : Byte;
    to_be_sent : Natural_M32 := 0;
    --  to_be_sent is not always equal to lzb'Length: sometimes you have a DL code
    mid : LZ_buffer_index_type;
  begin
    for i in lzb'Range loop
      case lzb (i).kind is
        when plain_byte =>
          to_be_sent := to_be_sent + 1;
        when distance_length =>
          to_be_sent := to_be_sent + Natural_M32 (lzb (i).lz_length);
      end case;
    end loop;
    if to_be_sent > 16#FFFF# then  --  Ow, cannot send all that in one chunk.
      --  Instead of a tedious block splitting, just divide and conquer:
      mid := LZ_buffer_index_type ((Natural_M32 (lzb'First) + Natural_M32 (lzb'Last)) / 2);
      if trace then
        Put_Line (log,
          "Expand_LZ_buffer: splitting large stored block: " &
          LZ_buffer_index_type'Image (lzb'First) &
          LZ_buffer_index_type'Image (mid) &
          LZ_buffer_index_type'Image (lzb'Last)
        );
      end if;
      Expand_LZ_buffer (lzb (lzb'First .. mid), last_block => False);
      Expand_LZ_buffer (lzb (mid + 1   .. lzb'Last), last_block => last_block);
      return;
    end if;
    if trace then
      Put_Line (log, "Expand_LZ_buffer: sending" & Natural_M32'Image (to_be_sent) & " 'plain' bytes");
    end if;
    b1 := Byte (to_be_sent mod 256);
    b2 := Byte (to_be_sent  /  256);
    Mark_new_block (last_block_for_stream => last_block);
    last_block_type := stored;
    Put_code (code => 0, code_size => 2);  --  Signals a "stored" block
    Flush_bit_buffer;  --  Go to byte boundary
    Put_byte (b1);
    Put_byte (b2);
    Put_byte (not b1);
    Put_byte (not b2);
    for i in lzb'Range loop
      case lzb (i).kind is
        when plain_byte =>
          Put_byte (lzb (i).plain);
        when distance_length =>
          for j in 1 .. lzb (i).lz_length loop
            Put_byte (lzb (i).lz_expanded (j));
          end loop;
      end case;
    end loop;
  end Expand_LZ_buffer;

  --  Extra bits that need to be sent after various Deflate codes

  extra_bits_for_lz_length_code : constant array (257 .. 285) of Natural :=
     (257 .. 264 => 0,
      265 .. 268 => 1,
      269 .. 272 => 2,
      273 .. 276 => 3,
      277 .. 280 => 4,
      281 .. 284 => 5,
      285        => 0
    );

  extra_bits_for_lz_distance_code : constant array (0 .. 29) of Natural :=
     (0 ..  3 =>  0,
      4 ..  5 =>  1,
      6 ..  7 =>  2,
      8 ..  9 =>  3,
     10 .. 11 =>  4,
     12 .. 13 =>  5,
     14 .. 15 =>  6,
     16 .. 17 =>  7,
     18 .. 19 =>  8,
     20 .. 21 =>  9,
     22 .. 23 => 10,
     24 .. 25 => 11,
     26 .. 27 => 12,
     28 .. 29 => 13
    );

  subtype Long_length_codes is
    Alphabet_lit_len range code_for_max_expand + 1 .. Alphabet_lit_len'Last;
  zero_bl_long_lengths : constant Stats_type (Long_length_codes) := (others => 0);

  --  Send_as_block.
  --
  --  lzb (can be a slice of the principal buffer) will be sent as:
  --        * a new "dynamic" block, preceded by a compression structure header
  --    or  * the continuation of previous "dynamic" block
  --    or  * a new "fixed" block, if lz data's Huffman descriptor is close enough to "fixed"
  --    or  * a new "stored" block, if lz data are too random

  procedure Send_as_block (lzb : LZ_buffer_type; last_block : Boolean) is
    new_descr, new_descr_2 : Deflate_Huff_descriptors;
    --
    procedure Send_fixed_block is
    begin
      if last_block_type = fixed then
        --  Cool, we don't need to mark a block boundary: the Huffman codes are already
        --  the expected ones. We can just continue sending the LZ atoms.
        null;
      else
        Mark_new_block (last_block_for_stream => last_block);
        curr_descr := Deflate_fixed_descriptors;
        Put_code (code => 1, code_size => 2);  --  Signals a "fixed" block
        last_block_type := fixed;
      end if;
      Put_LZ_buffer (lzb);
    end Send_fixed_block;
    --
    stats_lit_len, stats_lit_len_2 : Stats_lit_len_type;
    stats_dis, stats_dis_2 : Stats_dis_type;
    --
    procedure Send_dynamic_block (dyn : Deflate_Huff_descriptors) is
      dummy : Count_type := 0;
    begin
      Mark_new_block (last_block_for_stream => last_block);
      curr_descr := Prepare_Huffman_codes (dyn);
      Put_code (code => 2, code_size => 2);  --  Signals a "dynamic" block
      Put_compression_structure (curr_descr, cost_analysis => False, bits => dummy);
      Put_LZ_buffer (lzb);
      last_block_type := dynamic;
    end Send_dynamic_block;
    --  The following variables will contain the *exact* number of bits taken
    --  by the block to be sent, using different Huffman encodings, or stored.
    stored_format_bits,                      --  Block is stored (no compression)
    fixed_format_bits,                       --  Fixed (preset) Huffman codes
    dynamic_format_bits,                     --  Dynamic Huffman codes using block's statistics
    dynamic_format_bits_2,                   --  Dynamic Huffman codes after Tweak_for_better_RLE
    recycled_format_bits : Count_type := 0;  --  Continue previous block, use current Huffman codes
    --
    stored_format_possible : Boolean;  --  Can we store (needs expansion of DL codes) ?
    recycling_possible     : Boolean;  --  Can we recycle current Huffman codes ?
    --
    procedure Compute_sizes_of_variants is
      c     : Count_type;
      extra : Natural;
    begin
      --  We count bits taken by literals, for each block format variant.
      for i in 0 .. 255 loop
        c := stats_lit_len (i);  --  This literal appears c times in the LZ buffer
        stored_format_bits    := stored_format_bits    + 8                                               * c;
        fixed_format_bits     := fixed_format_bits     + Count_type (default_lit_len_bl (i))             * c;
        dynamic_format_bits   := dynamic_format_bits   + Count_type (new_descr.lit_len (i).bit_length)   * c;
        dynamic_format_bits_2 := dynamic_format_bits_2 + Count_type (new_descr_2.lit_len (i).bit_length) * c;
        recycled_format_bits  := recycled_format_bits  + Count_type (curr_descr.lit_len (i).bit_length)  * c;
      end loop;
      --  We count bits taken by DL codes.
      if stored_format_possible then
        for i in lzb'Range loop
          case lzb (i).kind is
            when plain_byte =>
              null;  --  Already counted
            when distance_length =>
               --  In the stored format, DL codes are expanded
              stored_format_bits := stored_format_bits + 8 * Count_type (lzb (i).lz_length);
          end case;
        end loop;
      end if;
      --  For compressed formats, count Huffman bits and extra bits.
      --  Lengths codes:
      for i in 257 .. 285 loop
        c := stats_lit_len (i);  --  This length code appears c times in the LZ buffer
        extra := extra_bits_for_lz_length_code (i);
        fixed_format_bits     := fixed_format_bits     + Count_type (default_lit_len_bl (i) + extra)             * c;
        dynamic_format_bits   := dynamic_format_bits   + Count_type (new_descr.lit_len (i).bit_length + extra)   * c;
        dynamic_format_bits_2 := dynamic_format_bits_2 + Count_type (new_descr_2.lit_len (i).bit_length + extra) * c;
        recycled_format_bits  := recycled_format_bits  + Count_type (curr_descr.lit_len (i).bit_length + extra)  * c;
      end loop;
      --  Distance codes:
      for i in 0 .. 29 loop
        c := stats_dis (i);  --  This distance code appears c times in the LZ buffer
        extra := extra_bits_for_lz_distance_code (i);
        fixed_format_bits     := fixed_format_bits     + Count_type (default_dis_bl (i) + extra)             * c;
        dynamic_format_bits   := dynamic_format_bits   + Count_type (new_descr.dis (i).bit_length + extra)   * c;
        dynamic_format_bits_2 := dynamic_format_bits_2 + Count_type (new_descr_2.dis (i).bit_length + extra) * c;
        recycled_format_bits  := recycled_format_bits  + Count_type (curr_descr.dis (i).bit_length + extra)  * c;
      end loop;
      --  Supplemental bits to be counted
      --
      stored_format_bits := stored_format_bits +
        (1 + (stored_format_bits / 8) / 65_535)  --  Number of stored blocks needed
        * 5  -- 5 bytes per header
        * 8; -- ... converted into bits
      --
      c := 1;  --  Is-last-block flag
      if block_to_finish and last_block_type in fixed .. dynamic then
        c := c + Count_type (curr_descr.lit_len (End_Of_Block).bit_length);
      end if;
      stored_format_bits    := stored_format_bits + c;
      fixed_format_bits     := fixed_format_bits + c + 2;
      dynamic_format_bits   := dynamic_format_bits + c + 2;
      dynamic_format_bits_2 := dynamic_format_bits_2 + c + 2;
      --  For both dynamic formats, we also counts the bits taken by the compression header!
      Put_compression_structure (new_descr,   cost_analysis => True, bits => dynamic_format_bits);
      Put_compression_structure (new_descr_2, cost_analysis => True, bits => dynamic_format_bits_2);
    end Compute_sizes_of_variants;
    --
    optimal_format_bits : Count_type;
  begin
    Get_statistics (lzb, stats_lit_len, stats_dis);
    new_descr := Build_descriptors (stats_lit_len, stats_dis);
    stats_lit_len_2 := stats_lit_len;
    stats_dis_2 := stats_dis;
    Tweak_for_better_RLE (stats_lit_len_2);
    Tweak_for_better_RLE (stats_dis_2);
    new_descr_2 := Build_descriptors (stats_lit_len_2, stats_dis_2);
    --  For "stored" block format, prevent expansion of DL codes with length > max_expand.
    --  We check stats are all 0 for long length codes:
    stored_format_possible := stats_lit_len (Long_length_codes) = zero_bl_long_lengths;
    recycling_possible :=
      last_block_type = fixed  --  The "fixed" alphabets use all symbols, then always recyclable.
        or else
      (last_block_type = dynamic and then Recyclable (curr_descr, new_descr));
    Compute_sizes_of_variants;
    if not stored_format_possible then
      stored_format_bits := Count_type'Last;
    end if;
    if not recycling_possible then
      recycled_format_bits := Count_type'Last;
    end if;
    optimal_format_bits := Count_type'Min (
      Count_type'Min (stored_format_bits, fixed_format_bits),
      Count_type'Min (
        Count_type'Min (dynamic_format_bits, dynamic_format_bits_2),
        recycled_format_bits)
    );
    --
    --  Selection of the block format with smallest size.
    --
    if fixed_format_bits = optimal_format_bits then
      if trace then
        Put_Line (log, "### New ""fixed"" block");
      end if;
      Send_fixed_block;
    elsif dynamic_format_bits = optimal_format_bits then
      if trace then
        Put_Line (log, "### New ""dynamic"" block with compression structure header");
      end if;
      Send_dynamic_block (new_descr);
    elsif dynamic_format_bits_2 = optimal_format_bits then
      if trace then
        Put_Line (log, "### New ""dynamic"" block, RLE-tweaked, with compression structure header");
      end if;
      Send_dynamic_block (new_descr_2);
    elsif recycled_format_bits = optimal_format_bits then
      if trace then
        Put_Line (log, "### Recycle: continue using existing Huffman compression structures");
      end if;
      Put_LZ_buffer (lzb);
    else  --  We have stored_format_bits = optimal_format_bits
      if trace then
        Put_Line (log, "### Too random - use ""stored"" block");
      end if;
      Expand_LZ_buffer (lzb, last_block);
    end if;
  end Send_as_block;

  subtype Full_range_LZ_buffer_type is LZ_buffer_type (LZ_buffer_index_type);
  type p_Full_range_LZ_buffer_type is access Full_range_LZ_buffer_type;
  procedure Dispose is
    new Ada.Unchecked_Deallocation (Full_range_LZ_buffer_type, p_Full_range_LZ_buffer_type);

  --  This is the main, big, fat, circular buffer containing LZ codes,
  --  each LZ code being a literal or a DL code.
  --  Heap allocation is needed only because default stack is too small on some targets.
  lz_buffer : p_Full_range_LZ_buffer_type := null;
  lz_buffer_index : LZ_buffer_index_type := 0;
  past_lz_data : Boolean := False;
  --  When True: some LZ_buffer_size data before lz_buffer_index (modulo!) are real, past data

  ---------------------------------------------------------------------------------
  --  Scanning and sampling: the really sexy part of the Taillaule algorithm...  --
  ---------------------------------------------------------------------------------

  --  We examine similarities in the LZ data flow at different step sizes.
  --  If the optimal Huffman encoding for this portion is very different, we choose to
  --  cut current block and start a new one. The shorter the step, the higher the threshold
  --  for starting a dynamic block, since the compression header is taking some room each time.

  --  *Tuned* (a bit...)
  min_step : constant := 750;

  type Step_threshold_metric is record
    slider_step       : LZ_buffer_index_type;  --  Should be a multiple of min_step.
    cutting_threshold : Positive;
    metric            : Distance_type;
  end record;

  --  *Tuned* thresholds
  --  NB: the enwik8, then silesia, then others tests are tough for lowering any!
  step_choice : constant array (Positive range <>) of Step_threshold_metric :=
     ((8 * min_step,  420, L1_tweaked),  --  Deflate_1, Deflate_2, Deflate_3 (enwik8)
      (4 * min_step,  430, L1_tweaked),  --             Deflate_2, Deflate_3 (silesia)
          (min_step, 2050, L1_tweaked)   --                        Deflate_3 (DB test)
    );

  max_choice : constant array (Taillaule_Deflation_Method) of Positive :=
    (Deflate_1 => 1, Deflate_2 => 2, others => step_choice'Last);

  slider_size : constant := 4096;
  half_slider_size : constant := slider_size / 2;
  slider_max : constant := slider_size - 1;

  --  Phases (A) and (B) are done in a single function: we get Huffman
  --  descriptors that should be good for encoding a given sequence of LZ atoms.
  function Build_descriptors (lzb : LZ_buffer_type) return Deflate_Huff_descriptors is
    stats_lit_len : Stats_lit_len_type;
    stats_dis     : Stats_dis_type;
  begin
    Get_statistics (lzb, stats_lit_len, stats_dis);
    return Build_descriptors (stats_lit_len, stats_dis);
  end Build_descriptors;

  procedure Scan_and_send_from_main_buffer (from, to : LZ_buffer_index_type; last_flush : Boolean) is
    --  The following descriptors are *not* used for compressing, but for detecting similarities.
    initial_hd, sliding_hd : Deflate_Huff_descriptors;
    start, slide_mid, send_from : LZ_buffer_index_type;
    sliding_hd_computed : Boolean;
  begin
    if to - from < slider_max then
      Send_as_block (lz_buffer (from .. to), last_flush);
      return;
    end if;
    --  For further comments: n := LZ_buffer_size
    if past_lz_data then  --  We have n / 2 previous data before 'from'.
      start := from - LZ_buffer_index_type (half_slider_size);
    else
      start := from;  --  Cannot have past data
    end if;
    if start > from then  --  Looped over, (mod n). Slider data are in two chunks in main buffer
      --  put_line(from'img & to'img & start'img);
      declare
        copy_from : LZ_buffer_index_type := start;
        copy : LZ_buffer_type (0 .. slider_max);
      begin
        for i in copy'Range loop
          copy (i) := lz_buffer (copy_from);
          copy_from := copy_from + 1;  --  Loops over (mod n)
        end loop;
        initial_hd := Build_descriptors (copy);
      end;
      --  Concatenation instead of above loop bombs with a Range Check error:
      --  lz_buffer(start .. lz_buffer'Last) &
      --  lz_buffer(0 .. start + LZ_buffer_index_type(slider_max))
    else
      initial_hd := Build_descriptors (lz_buffer (start .. start + slider_max));
    end if;
    send_from := from;
    slide_mid := from + min_step;
    Scan_LZ_data :
    while Integer_M32 (slide_mid) + half_slider_size < Integer_M32 (to) loop
      exit Scan_LZ_data when deactivate_scanning;
      sliding_hd_computed := False;
      Browse_step_level :
      for level in step_choice'Range loop
        exit Browse_step_level when level > max_choice (method);
        if (slide_mid - from) mod step_choice (level).slider_step = 0 then
          if not sliding_hd_computed then
            sliding_hd := Build_descriptors (lz_buffer (slide_mid - half_slider_size .. slide_mid + half_slider_size));
            sliding_hd_computed := True;
          end if;
          if not Similar (
            initial_hd,
            sliding_hd,
            step_choice (level).metric,
            step_choice (level).cutting_threshold,
            "Compare sliding to initial (step size=" &
            LZ_buffer_index_type'Image (step_choice (level).slider_step) & ')'
          )
          then
            if trace then
              Put_Line (log,
                "### Cutting @ " & LZ_buffer_index_type'Image (slide_mid) &
                "  ('from' is" & LZ_buffer_index_type'Image (from) &
                ", 'to' is" & LZ_buffer_index_type'Image (to) & ')'
              );
            end if;
            Send_as_block (lz_buffer (send_from .. slide_mid - 1), last_block => False);
            send_from := slide_mid;
            initial_hd := sliding_hd;  --  Reset reference descriptor for further comparisons
            exit Browse_step_level;  --  Cutting once at a given place is enough :-)
          end if;
        end if;
      end loop Browse_step_level;
      --  Exit before an eventual increment of slide_mid that would loop over (mod n).
      exit Scan_LZ_data when Integer_M32 (slide_mid) + min_step + half_slider_size >= Integer_M32 (to);
      slide_mid := slide_mid + min_step;
    end loop Scan_LZ_data;
    --
    --  Send last block for slice from .. to.
    --
    if send_from <= to then
      Send_as_block (lz_buffer (send_from .. to), last_block => last_flush);
    end if;
  end Scan_and_send_from_main_buffer;

  procedure Flush_half_buffer (last_flush : Boolean) is
    last_idx : constant LZ_buffer_index_type := lz_buffer_index - 1;
    n_div_2 : constant := LZ_buffer_size / 2;
  begin
    if last_idx < n_div_2 then
      Scan_and_send_from_main_buffer (0, last_idx, last_flush);        -- 1st half
    else
      Scan_and_send_from_main_buffer (n_div_2, last_idx, last_flush);  -- 2nd half
    end if;
    --  From this point, all further calls to Flush_half_buffer will
    --  have n_div_2 elements of past data.
    past_lz_data := True;
  end Flush_half_buffer;

  procedure Push (a : LZ_atom) is
  pragma Inline (Push);
  begin
    lz_buffer (lz_buffer_index) := a;
    lz_buffer_index := lz_buffer_index + 1;  --  becomes 0 when reaching LZ_buffer_size (modular)
    if lz_buffer_index * 2 = 0 then
      Flush_half_buffer (last_flush => False);
    end if;
  end Push;

  procedure Put_or_delay_literal_byte (b : Byte) is
  pragma Inline (Put_or_delay_literal_byte);
  begin
    case method is
      when Deflate_Fixed =>
        Put_literal_byte (b);  --  Buffering is not needed in this mode
      when Taillaule_Deflation_Method =>
        Push ((plain_byte, b, 0, 0, (b, others => 0)));
    end case;
  end Put_or_delay_literal_byte;

  procedure Put_or_delay_DL_code (distance, length : Integer; expand : Expanded_data) is
  pragma Inline (Put_or_delay_DL_code);
  begin
    case method is
      when Deflate_Fixed =>
        Put_DL_code (distance, length);  --  Buffering is not needed in this mode
      when Taillaule_Deflation_Method =>
        Push ((distance_length, 0, distance, length, expand));
    end case;
  end Put_or_delay_DL_code;

  --------------------------------
  -- LZ77 front-end compression --
  --------------------------------

  procedure Encode is

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
    begin
      if IO_buffers.InBufIdx > IO_buffers.MaxInBufIdx then
        Read_Block (IO_buffers, input);
      end if;
      return not IO_buffers.InputEoF;
    end More_bytes;

    --  LZ77 parameters
    Look_Ahead_LZ77    : constant Integer := 258;
    String_buffer_size : constant := 2**15;  --  Required: 2**15 for Deflate, 2**16 for Deflate_e
    type Text_buffer_index is mod String_buffer_size;
    type Text_buffer is array (Text_buffer_index) of Byte;
    Text_Buf : Text_buffer;
    R : Text_buffer_index;

    --  If the DLE coding doesn't fit the format constraints, we need
    --  to decode it as a simple sequence of literals. The buffer used is
    --  called "Text" buffer by reference to "clear-text", but actually it
    --  is any binary data.

    procedure LZ77_emits_DL_code (distance, length : Integer) is
      --  NB: no worry, all arithmetics in Text_buffer_index are modulo String_buffer_size.
      b : Byte;
      copy_start : Text_buffer_index;
      expand : Expanded_data;
      ie : Positive := 1;
    begin
      if distance = String_buffer_size then  --  Happens with 7-Zip, cannot happen with Info-Zip.
        copy_start := R;
      else
        copy_start := R - Text_buffer_index (distance);
      end if;
      --  Expand into the circular text buffer to have it up to date
      for K in 0 .. Text_buffer_index (length - 1) loop
        b := Text_Buf (copy_start + K);
        Text_Buf (R) := b;
        R := R + 1;
        if ie <= max_expand then  --  Also memorize short sequences for LZ buffer
          expand (ie) := b;       --  for the case a block needs to be stored in clear.
          ie := ie + 1;
        end if;
      end loop;
      if distance in Distance_range and length in Length_range then
        Put_or_delay_DL_code (distance, length, expand);
      else
        if trace then
          Put_Line (log,
            "<> Too bad, cannot encode this distance-length pair, " &
            "then we have to expand to output (dist = " & Integer'Image (distance) &
            " len=" & Integer'Image (length) & ")"
          );
        end if;
        for K in 0 .. Text_buffer_index (length - 1) loop
          Put_or_delay_literal_byte (Text_Buf (copy_start + K));
        end loop;
      end if;
    end LZ77_emits_DL_code;

    procedure LZ77_emits_literal_byte (b : Byte) is
    begin
      Text_Buf (R) := b;
      R := R + 1;
      Put_or_delay_literal_byte (b);
    end LZ77_emits_literal_byte;

    procedure Dummy_Estimate_DL_Codes (
      matches          : in out LZ77.Matches_Array;
      old_match_index  : in     Natural;
      prefixes         : in     LZ77.Byte_Array;
      best_score_index :    out Positive;
      best_score_set   :    out LZ77.Prefetch_Index_Type;
      match_trace      :    out LZ77.DLP_Array
    )
    is null;

    LZ77_choice : constant array (Deflation_Method) of LZ77.Method_Type :=
      (Deflate_Fixed  => LZ77.IZ_4,
       Deflate_0      => LZ77.No_LZ77,
       Deflate_1      => LZ77.IZ_6,     --  level 6 is the default in Info-Zip's zip.exe
       Deflate_2      => LZ77.IZ_8,
       Deflate_3      => LZ77.IZ_10,
       Deflate_R      => LZ77.Rich);

    procedure My_LZ77 is
      new LZ77.Encode
         (String_buffer_size => String_buffer_size,
          Look_Ahead         => Look_Ahead_LZ77,
          Threshold          => 2,  --  From a string match length > 2, a DL code is sent
          Method             => LZ77_choice (method),
          Read_Byte          => Read_byte,
          More_Bytes         => More_bytes,
          Write_Literal      => LZ77_emits_literal_byte,
          Write_DL_Code      => LZ77_emits_DL_code,
          Estimate_DL_Codes  => Dummy_Estimate_DL_Codes
        );

  begin  --  Encode
    Read_Block (IO_buffers, input);
    R := Text_buffer_index (String_buffer_size - Look_Ahead_LZ77);
    if input_size_known then
      feedback_milestone := Zip_Streams.ZS_Size_Type (input_size / feedback_steps);
    end if;
    case method is
      when Deflate_Fixed =>  --  "Fixed" (predefined) compression structure
        --  We have only one compressed data block, then it is already the last one.
        Put_code (code => 1, code_size => 1);  --  Signals last block
        Put_code (code => 1, code_size => 2);  --  Signals a "fixed" block
      when Taillaule_Deflation_Method =>
        null;  --  No start data sent, all is delayed
    end case;
    ----------------------------------------------------------------
    --  The whole compression is happening in the following line: --
    ----------------------------------------------------------------
    My_LZ77;
    --  Done. Send the code signaling the end of compressed data block:
    case method is
      when Deflate_Fixed =>
        Put_Huffman_code (curr_descr.lit_len (End_Of_Block));
      when Taillaule_Deflation_Method =>
        if lz_buffer_index * 2 = 0 then  --  Already flushed at latest Push, or empty data
          if block_to_finish and then last_block_type in fixed .. dynamic then
            Put_Huffman_code (curr_descr.lit_len (End_Of_Block));
          end if;
        else
          Flush_half_buffer (last_flush => True);
          if last_block_type in fixed .. dynamic then
            Put_Huffman_code (curr_descr.lit_len (End_Of_Block));
          end if;
        end if;
        if not last_block_marked then
          --  Add a fake fixed block, just to have a final block...
          Put_code (code => 1, code_size => 1);  --  Signals last block
          Put_code (code => 1, code_size => 2);  --  Signals a "fixed" block
          curr_descr := Deflate_fixed_descriptors;
          Put_Huffman_code (curr_descr.lit_len (End_Of_Block));
        end if;
    end case;
  end Encode;

  procedure Deallocation is
  begin
    Dispose (lz_buffer);
    Deallocate_Buffers (IO_buffers);
  end Deallocation;

begin
  if trace then
    begin
      Open (log, Append_File, log_name);
    exception
      when Name_Error =>
        Create (log, Out_File, log_name);
    end;
    Put (log, "New stream" & sep & sep & sep & sep & sep & sep & sep & sep);
    if input_size_known then
      Put (log, sep & Zip_32_Data_Size_Type'Image (input_size) &
               sep & sep & sep & sep & sep & sep & "bytes input");
    end if;
    New_Line (log);
  end if;
  Allocate_Buffers (IO_buffers, input_size_known, input_size);
  output_size := 0;
  lz_buffer := new Full_range_LZ_buffer_type;
  begin
    Encode;
    compression_ok := True;
    Flush_bit_buffer;
    Flush_byte_buffer;
  exception
    when Compression_inefficient =>  --  Escaped from Encode
      compression_ok := False;
  end;
  if trace then
    Close (log);
  end if;
  Deallocation;
exception
  when others =>
    Deallocation;
    raise;
end Zip.Compress.Deflate;
