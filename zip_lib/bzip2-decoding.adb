--  Legal licensing note:

--  Copyright (c) 2009 .. 2024 Gautier de Montmollin (maintainer of the Ada version)
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

--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

--  Translated on 20-Oct-2009 by (New) P2Ada v. 15-Nov-2006
--  Rework by G. de Montmollin (see spec. for details)

with Ada.Text_IO, Ada.Unchecked_Deallocation;

package body BZip2.Decoding is

  procedure Decompress is

    --------------------------
    --  Byte & Bit buffers  --
    --------------------------

    bits_available : Natural := 0;
    read_data : Byte := 0;
    use Interfaces;

    function Get_Bits (n : Natural) return Byte is
      result_get_bits : Byte;
      data : Byte;
    begin
      if n > bits_available then
        data := Read_Byte;
        result_get_bits := Shift_Right (read_data, 8 - n) or Shift_Right (data, 8 - (n - bits_available));
        read_data := Shift_Left (data, n - bits_available);
        bits_available := bits_available + 8;
      else
        result_get_bits := Shift_Right (read_data, 8 - n);
        read_data := Shift_Left (read_data, n);
      end if;
      bits_available := bits_available - n;
      return result_get_bits;
    end Get_Bits;

    function Get_Bits_32 (n : Natural) return Unsigned_32 is
    begin
      return Unsigned_32 (Get_Bits (n));
    end Get_Bits_32;

    function Get_Boolean return Boolean is
    begin
      return Boolean'Val (Get_Bits (1));
    end Get_Boolean;

    function Get_Byte return Byte is
    begin
      return Get_Bits (8);
    end Get_Byte;

    function Get_Cardinal_24 return Unsigned_32 is
    begin
      return Shift_Left (Get_Bits_32 (8), 16) or Shift_Left (Get_Bits_32 (8), 8) or Get_Bits_32 (8);
    end Get_Cardinal_24;

    function Get_Cardinal_32 return Unsigned_32 is
    begin
      return Shift_Left (Get_Bits_32 (8), 24)  or
             Shift_Left (Get_Bits_32 (8), 16)  or
             Shift_Left (Get_Bits_32 (8), 8)  or
             Get_Bits_32 (8);
    end Get_Cardinal_32;

    seq_to_unseq : array (0 .. 255) of Natural;
    inuse_count : Natural;

    --  Receive the mapping table. To save space, the in_use set is stored in pieces of 16 bits.
    --  First 16 bits store which pieces of 16 bits are used, then the pieces follow.
    procedure Receive_Mapping_Table is
      in_use : array (0 .. 15) of Boolean;
    begin
      --  Receive the first 16 bits which tell which pieces are stored.
      for i in in_use'Range loop
        in_use (i) := Get_Boolean;
      end loop;
      --  Receive the used pieces.
      inuse_count := 0;
      for i in in_use'Range loop
        if in_use (i) then
          for j in 0 .. 15 loop
            if Get_Boolean then
              seq_to_unseq (inuse_count) := 16 * i + j;
              inuse_count := inuse_count + 1;
            end if;
          end loop;
        end if;
      end loop;
    end Receive_Mapping_Table;

    entropy_coder_count : Byte;
    selector_count : Natural;
    selector, selector_mtf : array (0 .. max_selectors) of Byte;

    trace : constant Boolean := False;

    procedure Receive_Selectors is
      value : array (Byte range 0 .. max_entropy_coders - 1) of Byte;
      j, tmp, v : Byte;
    begin

      entropy_coder_count := Get_Bits (3);
      if entropy_coder_count not in min_entropy_coders .. max_entropy_coders then
        raise data_error
          with
            "Invalid BZip2 entropy coder count:" & entropy_coder_count'Image &
            ", should be between" & min_entropy_coders'Image & " and" & max_entropy_coders'Image;
      end if;
      selector_count := Natural (Shift_Left (Get_Bits_32 (8), 7) or Get_Bits_32 (7));  --  Up to 32767.
      if selector_count > max_selectors then
        raise data_error with "Invalid BZip2 selector count, maximum is" & max_selectors'Image;
        --  With standard settings, the maximum value is 18002.
      end if;

      if trace then
        Ada.Text_IO.Put_Line ("Entropy coders:" & entropy_coder_count'Image);
        Ada.Text_IO.Put_Line ("Selectors: . . " & selector_count'Image);
      end if;

      --  1) Receive selector list, MTF-transformed:
      for i in 0 .. selector_count - 1 loop
        j := 0;
        while Get_Boolean loop
          j := j + 1;
          if j > 5 then
            raise data_error
              with
                "Invalid BZip2 entropy coder index, maximum is" &
                Integer'Image (max_entropy_coders - 1);
          end if;
        end loop;
        selector_mtf (i) := j;
      end loop;

      --  2) De-transform selectors list:
      for w in Byte range 0 .. entropy_coder_count - 1 loop
        --  We start with 0, 1, 2, 3, ...:
        value (w) := w;
      end loop;
      Undo_MTF_Values_For_Selectors :
      for i in 0 .. selector_count - 1 loop
        v := selector_mtf (i);
        --  Move pos (v) to the front.
        tmp := value (v);
        while v /= 0 loop
          value (v) := value (v - 1);
          v := v - 1;
        end loop;
        value (0) := tmp;
        selector (i) := tmp;
      end loop Undo_MTF_Values_For_Selectors;

    end Receive_Selectors;

    type Alphabet_U32_array is array (0 .. max_alphabet_size) of Unsigned_32;
    type Alphabet_Nat_array is array (0 .. max_alphabet_size) of Natural;

    procedure Create_Huffman_Decoding_Tables
      (limit, base, perm : in out Alphabet_U32_array;
       length            : in     Alphabet_Nat_array;
       min_len, max_len  : in     Natural;
       alphabet_size     : in     Integer)
    is
      pp, idx : Integer;
      vec : Unsigned_32;
    begin
      pp := 0;
      for i in min_len .. max_len loop
        for j in 0 .. alphabet_size - 1 loop
          if length (j) = i then
            perm (pp) := Unsigned_32 (j);
            pp := pp + 1;
          end if;
        end loop;
      end loop;
      for i in 0 .. max_code_len_max - 1 loop
        base (i) := 0;
        limit (i) := 0;
      end loop;
      for i in 0 .. alphabet_size - 1 loop
        idx := length (i) + 1;
        base (idx) := base (idx) + 1;
      end loop;
      for i in 1 .. max_code_len_max - 1 loop
        base (i) := base (i) + base (i - 1);
      end loop;
      vec := 0;
      for i in min_len .. max_len loop
        vec := vec + base (i + 1) - base (i);
        limit (i) := vec - 1;
        vec := vec * 2;
      end loop;
      for i in min_len + 1 .. max_len loop
        base (i) := (limit (i - 1) + 1) * 2 - base (i);
      end loop;
    end Create_Huffman_Decoding_Tables;

    type U32_Array is array (Natural_32 range <>) of Unsigned_32;
    type U32_Array_Access is access U32_Array;
    procedure Dispose is new Ada.Unchecked_Deallocation (U32_Array, U32_Array_Access);

    alphabet_size_overall : Natural;  --  Alphabet size used for all groups

    --  Tables for the Huffman trees used for decoding MTF values.
    limit, base, perm : array (Byte range 0 .. max_entropy_coders - 1) of Alphabet_U32_array;
    min_lens : array (Byte range 0 .. max_entropy_coders - 1) of Natural;
    len : array (Byte range 0 .. max_entropy_coders - 1) of Alphabet_Nat_array;

    procedure Receive_Huffman_Bit_Lengths is
      current_bit_length : Natural;
    begin
      for t in 0 .. entropy_coder_count - 1 loop
        current_bit_length := Natural (Get_Bits (5));
        if current_bit_length not in 1 .. max_code_len_bzip2_1_0_2 then
          raise data_error with
            "In BZip2 data, invalid initial bit length for a Huffman tree: got length" &
            current_bit_length'Image & "; range should be 1 .." & max_code_len_bzip2_1_0_2'Image;
        end if;
        for symbol in 0 .. alphabet_size_overall - 1 loop
          loop
            exit when not Get_Boolean;
            if Get_Boolean then
              current_bit_length := current_bit_length - 1;
            else
              current_bit_length := current_bit_length + 1;
            end if;
          end loop;
          if current_bit_length not in 1 .. max_code_len_bzip2_1_0_2 then
            raise data_error with
              "In BZip2 data, invalid bit length for a Huffman tree: for symbol " &
              symbol'Image & " got length" &
              current_bit_length'Image & "; range should be 1 .." & max_code_len_bzip2_1_0_2'Image;
          end if;
          len (t)(symbol) := current_bit_length;
        end loop;
      end loop;
    end Receive_Huffman_Bit_Lengths;

    procedure Make_Huffman_Tables is
      min_len, max_len : Natural;
    begin
      for t in 0 .. entropy_coder_count - 1 loop
        min_len := 32;
        max_len := 0;
        for i in 0 .. alphabet_size_overall - 1 loop
          if len (t)(i) > max_len then
            max_len := len (t)(i);
          end if;
          if len (t)(i) < min_len then
            min_len := len (t)(i);
          end if;
        end loop;
        Create_Huffman_Decoding_Tables
          (limit (t), base (t), perm (t), len (t), min_len, max_len, alphabet_size_overall);
        min_lens (t) := min_len;
      end loop;
    end Make_Huffman_Tables;

    block_size : Natural_32;
    tt : U32_Array_Access;

    -------------------------
    -- MTF - Move To Front --
    -------------------------

    cf_tab : array (0 .. 257) of Natural_32;
    tt_count : Natural_32;

    procedure Receive_MTF_Values is
      --  NB: it seems that MTF is also performed in this procedure (where else?).
      mtf_a_size : constant := 4096;
      mtf_l_size : constant := 16;
      mtf_base : array (0 .. 256 / mtf_l_size - 1) of Natural;
      mtf_a : array (0 .. mtf_a_size - 1) of Natural;
      --
      procedure Init_MTF is
        k : Natural := mtf_a_size - 1;
      begin
        for i in reverse 0 .. 256  /  mtf_l_size - 1 loop
          for j in reverse 0 .. mtf_l_size - 1 loop
            mtf_a (k) := i * mtf_l_size + j;
            k := k - 1;
          end loop;
          mtf_base (i) := k + 1;
        end loop;
      end Init_MTF;
      --
      pos_countdown, group_no : Integer;
      g_sel : Byte;
      g_min_len : Natural;
      --
      function Get_MTF_Value return Unsigned_32 is
        z_n : Natural;
        z_vec : Unsigned_32;
        perm_index : Integer;
      begin
        if pos_countdown = 0 then
          pos_countdown := group_size;
          group_no := group_no + 1;
          if group_no > selector_count - 1 then
            raise data_error
              with
                "In BZip2 data, selector index exceeds selector count," &
                selector_count'Image;
          end if;
          g_sel := selector (group_no);
          if g_sel not in base'Range then
            raise data_error
              with "In BZip2 data, invalid selector value," & g_sel'Image;
          end if;
          g_min_len := min_lens (g_sel);
        end if;
        pos_countdown := pos_countdown - 1;
        z_n := g_min_len;
        z_vec := Get_Bits_32 (z_n);
        while z_vec > limit (g_sel)(z_n) loop
          z_n := z_n + 1;
          z_vec := Shift_Left (z_vec, 1) or Get_Bits_32 (1);
        end loop;
        if z_n not in Alphabet_U32_array'Range then
          raise data_error with "In BZip2 data, invalid data in Huffman decoding [1]";
        end if;
        if z_vec > 2 ** (Integer'Size - 1) - 1 then
          raise data_error with "In BZip2 data, invalid data in Huffman decoding [2]";
        end if;
        perm_index := Integer (z_vec - base (g_sel)(z_n));
        if perm_index not in Alphabet_U32_array'Range then
          raise data_error with "In BZip2 data, invalid data in Huffman decoding [3]";
        end if;
        return perm (g_sel)(perm_index);
      end Get_MTF_Value;
      --
      procedure Move_MTF_Block is
        j, k : Natural;
      begin
        k := mtf_a_size;
        for i in reverse 0 .. 256  /  mtf_l_size - 1 loop
          j := mtf_base (i);
          mtf_a (k - 16 .. k - 1) := mtf_a (j .. j + 15);
          k := k - 16;
          mtf_base (i) := k;
        end loop;
      end Move_MTF_Block;
      --
      t : Natural_32;
      next_sym : Unsigned_32;
      es : Natural_32;
      n : Natural;
      p, q : Natural;  --  indexes mtf_a
      u, v : Natural;  --  indexes mtf_base
      lno, off : Natural;

      procedure Setup_Table is
      --  Setup cf_tab to facilitate generation of inverse transformation.
        t, nn : Natural_32;
      begin
        t := 0;
        for i in 0 .. 256 loop
          nn := cf_tab (i);
          cf_tab (i) := t;
          t := t + nn;
        end loop;
      end Setup_Table;

      nn : Natural;

    begin  --  Receive_MTF_Values
      group_no := -1;
      pos_countdown := 0;
      t := 0;
      cf_tab := (others => 0);
      Init_MTF;
      next_sym := Get_MTF_Value;
      --
      while Natural (next_sym) /= inuse_count + 1 loop
        if next_sym <= run_b then
          es := 0;
          n := 0;
          loop
            es := es + Natural_32 (Shift_Left (next_sym + 1, n));
            n := n + 1;
            next_sym := Get_MTF_Value;
            exit when next_sym > run_b;
          end loop;
          n := seq_to_unseq (mtf_a (mtf_base (0)));
          cf_tab (n) := cf_tab (n) + es;
          if t + es > sub_block_size * block_size then
            raise data_error;
          end if;
          while es > 0 loop
            tt (t) := Unsigned_32 (n);
            es := es - 1;
            t := t + 1;
          end loop;
        else
          --  NB: Likely, the reverse MTF algo happens here.
          nn := Natural (next_sym - 1);  --  Here we know: next_sym > 1, nn > 0.
          if nn < mtf_l_size then
            --  Avoid the costs of the general case.
            p := mtf_base (0);
            q := p + nn;  --  We know: q > p.
            n := mtf_a (q);
            loop
              mtf_a (q) := mtf_a (q - 1);
              q := q - 1;
              exit when q = p;
            end loop;
            mtf_a (q) := n;
          else
            --  General case.
            lno := nn   /   mtf_l_size;
            off := nn  mod  mtf_l_size;
            p := mtf_base (lno);
            q := p + off;  --  q >= p
            n := mtf_a (q);
            while q /= p loop
              mtf_a (q) := mtf_a (q - 1);
              q := q - 1;
            end loop;
            u := mtf_base'First;
            v := u + lno;
            loop
              mtf_a (mtf_base (v)) := mtf_a (mtf_base (v - 1) + mtf_l_size - 1);
              v := v - 1;
              mtf_base (v) := mtf_base (v) - 1;
              exit when v = u;
            end loop;
            mtf_a (mtf_base (v)) := n;
            if mtf_base (v) = 0 then
              Move_MTF_Block;
            end if;
          end if;
          cf_tab (seq_to_unseq (n)) := cf_tab (seq_to_unseq (n)) + 1;
          tt (t) := Unsigned_32 (seq_to_unseq (n));
          t := t + 1;
          if t > sub_block_size * block_size then
            raise data_error;
          end if;
          next_sym := Get_MTF_Value;
        end if;
      end loop;
      tt_count := t;
      Setup_Table;
    end Receive_MTF_Values;

    procedure BWT_Detransform is
      a : Unsigned_32 := 0;
      r : Natural_32;
      i255 : Natural;
    begin
      for p in 0 .. tt_count - 1 loop
        i255 := Natural (tt (p) and 16#ff#);
        r := cf_tab (i255);
        cf_tab (i255) := cf_tab (i255) + 1;
        tt (r) := tt (r) or a;
        a := a + 16#100#;
      end loop;
    end BWT_Detransform;

    computed_combined_crc, computed_block_crc : Unsigned_32;
    block_origin : Natural_32 := 0;

    block_counter : Natural := 0;

    procedure RLE_1 is
      decode_available : Natural_32 := Natural_32'Last;
      next_rle_idx : Integer_32 := -2;

      function RLE_Byte return Byte with Inline is
        b : Byte;
      begin
        if next_rle_idx not in tt'Range then
          raise data_error with "BZip2: invalid index for data output";
        end if;
        b := Byte (tt (next_rle_idx) and 16#FF#);
        next_rle_idx := Natural_32 (Shift_Right (tt (next_rle_idx), 8));
        decode_available := decode_available - 1;
        return b;
      end RLE_Byte;

      rle_len : Natural := 0;
      data, old_data : Byte := 0;

      procedure Flush_Run with Inline is
      begin
        for i in 1 .. rle_len loop
          Write_Byte (old_data);
          CRC.Update (computed_block_crc, old_data);
        end loop;
      end Flush_Run;

    begin
      decode_available := tt_count;
      next_rle_idx := Natural_32 (Shift_Right (tt (block_origin), 8));

      while decode_available > 0 loop
        --  On first iteration, because rle_len = 0, the run won't be
        --  flushed, then old_data being undefined is not an issue.
        data := RLE_Byte;
        if rle_len > 0 and then data /= old_data then
          --  Run break.
          Flush_Run;
          rle_len := 1;  --  New run with 1 element, data.
        else
          --  Length 0 (no old data), or old = new.
          rle_len := rle_len + 1;
          if rle_len = 4 then
            if decode_available > 0 then
              rle_len := rle_len + Natural (RLE_Byte);
            end if;
            Flush_Run;     --  Force a run break.
            rle_len := 0;  --  Run is empty at this point.
          end if;
        end if;
        old_data := data;
      end loop;
      Flush_Run;
    end RLE_1;

    --  Decode a new compressed block.
    function Decode_Block return Boolean is
      magic : String (1 .. 6);
      stored_crc : Unsigned_32;
      dummy : Boolean;
    begin
      for i in 1 .. 6 loop
        magic (i) := Character'Val (Get_Byte);
      end loop;
      if magic = block_header_magic then
        block_counter := block_counter + 1;
        if check_crc then
          CRC.Init (computed_block_crc);
        end if;
        stored_crc := Get_Cardinal_32;
        if trace then
          Ada.Text_IO.Put_Line ("Block CRC (stored):       " & stored_crc'Image);
        end if;
        dummy := Get_Boolean;  --  Randomized flag.
        block_origin := Natural_32 (Get_Cardinal_24);
        Receive_Mapping_Table;
        alphabet_size_overall := inuse_count + 2;
        Receive_Selectors;
        Receive_Huffman_Bit_Lengths;
        Make_Huffman_Tables;
        --  Move-to-Front:
        Receive_MTF_Values;
        --  Undo the Burrows Wheeler Transformation.
        BWT_Detransform;
        --
        RLE_1;
        --
        if trace then
          Ada.Text_IO.Put_Line ("Block CRC (computed):     " & computed_block_crc'Image);
        end if;
        if check_crc then
          computed_block_crc := CRC.Final (computed_block_crc);
          if computed_block_crc /= stored_crc then
            raise block_crc_check_failed
              with
                "BZip2: mismatch in block" & block_counter'Image &
                "'s CRC: computed =" & computed_block_crc'Image &
                ", stored =" & stored_crc'Image;
          end if;
          computed_combined_crc := Rotate_Left (computed_combined_crc, 1) xor computed_block_crc;
          if trace then
            Ada.Text_IO.Put_Line ("Combined CRC (computed):  " & computed_combined_crc'Image);
          end if;
        end if;
        return True;
      elsif magic = stream_footer_magic then
        stored_crc := Get_Cardinal_32;
        if check_crc and then stored_crc /= computed_combined_crc then
          raise block_crc_check_failed
            with
              "BZip2: mismatch in combined blocks' CRC: computed =" &
              computed_combined_crc'Image & "; stored =" & stored_crc'Image;
        end if;
        if trace then
          Ada.Text_IO.Put_Line ("Combined CRC (stored):    " & stored_crc'Image);
        end if;
        return False;
      else
        raise bad_block_magic with "BZip2: expecting block magic or stream footer";
      end if;
    end Decode_Block;

    procedure Init_Stream_Decompression is
      magic : String (1 .. 3);
      b : Byte;
    begin
      --  Read the magic.
      for i in magic'Range loop
        b := Read_Byte;
        magic (i) := Character'Val (b);
      end loop;
      if magic /= "BZh" then
        raise bad_header_magic;
      end if;
      --  Read the block size and allocate the working array.
      b := Read_Byte;
      if b not in Character'Pos ('1') .. Character'Pos ('9') then
        raise data_error with "Received bad BZip2 block size, should be in '1' .. '9'";
      end if;
      block_size := Natural_32 (b) - Character'Pos ('0');
      tt := new U32_Array (0 .. block_size * sub_block_size);
      computed_combined_crc := 0;
    end Init_Stream_Decompression;

  begin
    Init_Stream_Decompression;
    loop
      exit when not Decode_Block;
    end loop;
    Dispose (tt);
  end Decompress;

end BZip2.Decoding;
