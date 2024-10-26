--  Legal licensing note:

--  Copyright (c) 2009 .. 2019 Gautier de Montmollin (maintainer of the Ada version)
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

with Ada.Unchecked_Deallocation;

package body BZip2.Decoding is

  procedure Decompress is

    max_groups        : constant := 6;
    max_alphabet_size : constant := 258;
    max_code_len      : constant := 23;
    group_size        : constant := 50;
    max_selectors     : constant := 2 + (900_000 / group_size);

    sub_block_size : constant := 100_000;

    type Length_Array is array (Integer range <>) of Natural;

    use Interfaces;

    subtype Natural_32 is Integer_32 range 0 .. Integer_32'Last;

    block_randomized : Boolean := False;
    block_size : Natural_32;

    type Tcardinal_array is array (Natural_32 range <>) of Unsigned_32;
    type Pcardinal_array is access Tcardinal_array;
    procedure Dispose is new Ada.Unchecked_Deallocation (Tcardinal_array, Pcardinal_array);
    tt : Pcardinal_array;
    tt_count : Natural_32;

    rle_run_left : Natural := 0;
    rle_run_data : Byte := 0;
    decode_available : Natural_32 := Natural_32'Last;
    block_origin : Natural_32 := 0;
    read_data : Byte := 0;
    bits_available : Natural := 0;
    inuse_count : Natural;
    seq_to_unseq : array (0 .. 255) of Natural;
    alphabet_size_glob : Natural;  --  Not global actually, but less local than "alphabet_size" below
    group_count : Natural;
    --
    selector_count : Natural;
    selector, selector_mtf : array (0 .. max_selectors) of Byte;
    --
    type Alphabet_U32_array is array (0 .. max_alphabet_size) of Unsigned_32;
    type Alphabet_Nat_array is array (0 .. max_alphabet_size) of Natural;

    len          : array (0 .. max_groups) of Alphabet_Nat_array;
    limit_glob,  --  Not global actually, but less local than "limit" below
    base_glob,   --  Not global actually, but less local than "base" below
    perm_glob    --  Not global actually, but less local than "perm" below
                 : array (0 .. max_groups) of Alphabet_U32_array;
    --
    min_lens : Length_Array (0 .. max_groups);
    cf_tab : array (0 .. 257) of Natural_32;
    --
    end_reached : Boolean := False;

    in_buf : Buffer (1 .. input_buffer_size);
    in_idx : Natural := in_buf'Last + 1;

    function Read_byte return Byte is
      res : Byte;
    begin
      if in_idx > in_buf'Last then
        Read (in_buf);
        in_idx := in_buf'First;
      end if;
      res := in_buf (in_idx);
      in_idx := in_idx + 1;
      return res;
    end Read_byte;

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
      for i in 0 .. max_code_len - 1 loop
        base (i) := 0;
        limit (i) := 0;
      end loop;
      for i in 0 .. alphabet_size - 1 loop
        idx := length (i) + 1;
        base (idx) := base (idx) + 1;
      end loop;
      for i in 1 .. max_code_len - 1 loop
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

    procedure Init is
      magic : String (1 .. 3);
      b : Byte;
    begin
      --  Read the magic.
      for i in magic'Range loop
        b := Read_byte;
        magic (i) := Character'Val (b);
      end loop;
      if magic /= "BZh" then
        raise bad_header_magic;
      end if;
      --  Read the block size and allocate the working array.
      b := Read_byte;
      block_size := Natural_32 (b) - Character'Pos ('0');
      tt := new Tcardinal_array (0 .. block_size * sub_block_size);
    end Init;

    function Get_Bits (n : Natural) return Byte is
      Result_get_bits : Byte;
      data : Byte;
    begin
      if n > bits_available then
        data := Read_byte;
        Result_get_bits := Shift_Right (read_data, 8 - n) or Shift_Right (data, 8 - (n - bits_available));
        read_data := Shift_Left (data, n - bits_available);
        bits_available := bits_available + 8;
      else
        Result_get_bits := Shift_Right (read_data, 8 - n);
        read_data := Shift_Left (read_data, n);
      end if;
      bits_available := bits_available - n;
      return Result_get_bits;
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

    --  Receive the mapping table. To save space, the inuse set is stored in pieces
    --  of 16 bits. First 16 bits are stored which pieces of 16 bits are used, then
    --  the pieces follow.
    procedure Receive_Mapping_Table is
      inuse16 : array (0 .. 15) of Boolean;
      --* inuse: array(0 .. 255) of Boolean; -- for dump purposes
    begin
      --  Receive the first 16 bits which tell which pieces are stored.
      for i in inuse16'Range loop
        inuse16 (i) := Get_Boolean;
      end loop;
      --  Receive the used pieces.
      --* inuse:= (others => False);
      inuse_count := 0;
      for i in inuse16'Range loop
        if inuse16 (i) then
          for j in 0 .. 15 loop
            if Get_Boolean then
              --* inuse(16*i+j):= True;
              seq_to_unseq (inuse_count) := 16 * i + j;
              inuse_count := inuse_count + 1;
            end if;
          end loop;
        end if;
      end loop;
    end Receive_Mapping_Table;

    procedure Receive_Selectors is
      j : Byte;
    begin
      group_count := Natural (Get_Bits (3));
      selector_count := Natural (Shift_Left (Get_Bits_32 (8), 7) or Get_Bits_32 (7));
      for i in 0 .. selector_count - 1 loop
        j := 0;
        while Get_Boolean loop
          j := j + 1;
          if j > 5 then
            raise data_error;
          end if;
        end loop;
        selector_mtf (i) := j;
      end loop;
    end Receive_Selectors;

    procedure Undo_MTF_Values_For_Selectors is
      pos : array (0 .. max_groups) of Natural;
      v, tmp : Natural;
    begin
      for w in 0 .. group_count - 1 loop
        pos (w) := w;
      end loop;
      for i in 0 .. selector_count - 1 loop
        v := Natural (selector_mtf (i));
        tmp := pos (v);
        while v /= 0 loop
          pos (v) := pos (v - 1);
          v := v - 1;
        end loop;
        pos (0) := tmp;
        selector (i) := Byte (tmp);
      end loop;
    end Undo_MTF_Values_For_Selectors;

    procedure Receive_Huffman_Bit_Lengths is
      current_bit_length : Natural;
    begin
      for t in 0 .. group_count - 1 loop
        current_bit_length := Natural (Get_Bits (5));
        for symbol in 0 .. alphabet_size_glob - 1 loop
          loop
            if current_bit_length not in 1 .. 20 then
              raise data_error;
            end if;
            exit when not Get_Boolean;
            if Get_Boolean then
              current_bit_length := current_bit_length - 1;
            else
              current_bit_length := current_bit_length + 1;
            end if;
          end loop;
          len (t)(symbol) := current_bit_length;
        end loop;
      end loop;
    end Receive_Huffman_Bit_Lengths;

    procedure Make_Huffman_Tables is
      min_len, max_len : Natural;
    begin
      for t in 0 .. group_count - 1 loop
        min_len := 32;
        max_len := 0;
        for i in 0 .. alphabet_size_glob - 1 loop
          if len (t)(i) > max_len then
            max_len := len (t)(i);
          end if;
          if len (t)(i) < min_len then
            min_len := len (t)(i);
          end if;
        end loop;
        Create_Huffman_Decoding_Tables
          (limit_glob (t), base_glob (t), perm_glob (t), len (t),
           min_len, max_len, alphabet_size_glob);
        min_lens (t) := min_len;
      end loop;
    end Make_Huffman_Tables;

    -------------------------
    -- MTF - Move To Front --
    -------------------------

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
      group_pos, group_no : Integer;
      g_min_len, g_sel : Natural;
      --
      function Get_MTF_Value return Unsigned_32 is
        z_n : Natural;
        z_vec : Unsigned_32;
      begin
        if group_pos = 0 then
          group_no := group_no + 1;
          group_pos := group_size;
          g_sel := Natural (selector (group_no));
          g_min_len := min_lens (g_sel);
        end if;
        group_pos := group_pos - 1;
        z_n := g_min_len;
        z_vec := Get_Bits_32 (z_n);
        while z_vec > limit_glob (g_sel)(z_n) loop
          z_n := z_n + 1;
          z_vec := Shift_Left (z_vec, 1) or Get_Bits_32 (1);
        end loop;
        return perm_glob (g_sel)(Natural (z_vec - base_glob (g_sel)(z_n)));
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
      run_b : constant := 1;
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
      group_pos := 0;
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

    compare_final_CRC : Boolean := False;
    stored_blockcrc, mem_stored_blockcrc, computed_crc : Unsigned_32;

    --  Decode a new compressed block.
    function Decode_Block return Boolean is
      magic : String (1 .. 6);
    begin
      for i in 1 .. 6 loop
        magic (i) := Character'Val (Get_Byte);
      end loop;
      if magic = "1AY&SY" then
        if check_CRC then
          if compare_final_CRC then
            null;  --  initialisation is delayed until the rle buffer is empty
          else
            CRC.Init (computed_crc);  --  Initialize for next block.
          end if;
        end if;
        stored_blockcrc := Get_Cardinal_32;
        block_randomized := Get_Boolean;
        block_origin := Natural_32 (Get_Cardinal_24);
        Receive_Mapping_Table;
        alphabet_size_glob := inuse_count + 2;
        Receive_Selectors;
        Undo_MTF_Values_For_Selectors;
        Receive_Huffman_Bit_Lengths;
        Make_Huffman_Tables;
        --  Move-to-Front:
        Receive_MTF_Values;
        --  Undo the Burrows Wheeler Transformation.
        BWT_Detransform;
        decode_available := tt_count;
        return True;
      elsif magic = Character'Val (16#17#) & "rE8P" & Character'Val (16#90#) then
        return False;
      else
        raise bad_block_magic;
      end if;
    end Decode_Block;

    next_rle_idx : Integer_32 := -2;
    buf : Buffer (1 .. output_buffer_size);
    last : Natural;

    procedure Read_Chunk is
      shorten : Natural := 0;

      procedure RLE_Read is
        rle_len : Natural;
        data : Byte;
        idx : Integer := buf'First;
        count : Integer := buf'Length;
        --
        procedure RLE_Write is
          pragma Inline (RLE_Write);
        begin
          loop
            buf (idx) := data;
            idx := idx + 1;
            count := count - 1;
            rle_len := rle_len - 1;
            if check_CRC then
              CRC.Update (computed_crc, data);
              if rle_len = 0 and then compare_final_CRC then
                if CRC.Final (computed_crc) /= mem_stored_blockcrc then
                  raise block_crc_check_failed;
                end if;
                compare_final_CRC := False;
                CRC.Init (computed_crc);  --  Initialize for next block.
              end if;
            end if;
            exit when rle_len = 0 or count = 0;
          end loop;
        end RLE_Write;
        --
        --  Handle extreme cases of data of length 1, 2.
        --  This exception is always handled (see end of RLE_Read).
        input_dried : exception;
        --
        --  Make next_rle_idx index to the next decoded byte.
        --  If next_rle_idx did index to the last
        --  byte in the current block, decode the next block.
        --
        procedure Consume_RLE is
          pragma Inline (Consume_RLE);
        begin
          next_rle_idx := Natural_32 (Shift_Right (tt (next_rle_idx), 8));
          decode_available := decode_available - 1;
          if decode_available = 0 then
            compare_final_CRC := True;
            mem_stored_blockcrc := stored_blockcrc;
            --  ^ There might be a new block when last block's
            --    rle is finally emptied.
            --
            --  ** New block
            if Decode_Block then
              next_rle_idx := Natural_32 (Shift_Right (tt (block_origin), 8));
            else
              next_rle_idx := -1;
              end_reached := True;
            end if;
            --  **
            if end_reached then
              raise input_dried;
            end if;
          end if;
        end Consume_RLE;
        --
        function RLE_Byte return Byte is
          pragma Inline (RLE_Byte);
        begin
          return Byte (tt (next_rle_idx) and 16#FF#);
        end RLE_Byte;
        --
        function RLE_Possible return Boolean is
          pragma Inline (RLE_Possible);
        begin
          return decode_available > 0 and then data = RLE_Byte;
        end RLE_Possible;
        --
      begin  --  RLE_Read
        rle_len := rle_run_left;
        data := rle_run_data;
        if block_randomized then
          raise randomized_not_yet_implemented;
        end if;
        if rle_len /= 0 then
          RLE_Write;
          if count = 0 then
            shorten := 0;
            rle_run_data := data;
            rle_run_left := rle_len;
            return;
          end if;
        end if;
        begin
          Big_RLE_Loop :
          loop
            if decode_available = 0 or end_reached then
              exit Big_RLE_Loop;
            end if;
            rle_len := 1;
            data := RLE_Byte;
            Consume_RLE;
            if RLE_Possible then
              rle_len := rle_len + 1;
              Consume_RLE;
              if RLE_Possible then
                rle_len := rle_len + 1;
                Consume_RLE;
                if RLE_Possible then
                  Consume_RLE;
                  rle_len := rle_len + Natural (RLE_Byte) + 1;
                  Consume_RLE;
                end if;
              end if;
            end if;
            RLE_Write;
            exit Big_RLE_Loop when count = 0;
          end loop Big_RLE_Loop;
        exception
          when input_dried => RLE_Write;
        end;
        shorten := count;
        rle_run_data := data;
        rle_run_left := rle_len;
      end RLE_Read;

    begin
      last := buf'Last;
      if decode_available = Natural_32'Last then
        if Decode_Block then
          next_rle_idx := Natural_32 (Shift_Right (tt (block_origin), 8));
        else
          next_rle_idx := -1;
          end_reached := True;
        end if;
      end if;
      RLE_Read;
      last := last - shorten;
    end Read_Chunk;

  begin
    Init;
    loop
      Read_Chunk;
      Write (buf (1 .. last));
      exit when end_reached and rle_run_left = 0;
    end loop;
    Dispose (tt);
  end Decompress;

end BZip2.Decoding;
