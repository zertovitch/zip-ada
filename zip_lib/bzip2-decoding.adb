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

    max_groups     : constant := 6;
    max_alpha_size : constant := 258;
    max_code_len   : constant := 23;
    group_size     : constant := 50;
    max_selectors  : constant := 2 + (900_000 / group_size);

    sub_block_size : constant := 100_000;

    type Length_array is array (Integer range <>) of Natural;

    block_randomized : Boolean := False;
    block_size : Natural;

    use Interfaces;

    type Tcardinal_array is array (Integer range <>) of Unsigned_32;
    type Pcardinal_array is access Tcardinal_array;
    procedure Dispose is new Ada.Unchecked_Deallocation (Tcardinal_array, Pcardinal_array);
    tt : Pcardinal_array;
    tt_count : Natural;

    rle_run_left : Natural := 0;
    rle_run_data : Byte := 0;
    decode_available : Natural := Natural'Last;
    block_origin : Natural := 0;
    read_data : Byte := 0;
    bits_available : Natural := 0;
    inuse_count : Natural;
    seq_to_unseq : array (0 .. 255) of Natural;
    alpha_size_glob : Natural;  --  Not global actually, but less local than "alpha_size" below
    group_count : Natural;
    --
    selector_count : Natural;
    selector, selector_mtf : array (0 .. max_selectors) of Byte;
    --
    type Alpha_U32_array is array (0 .. max_alpha_size) of Unsigned_32;
    type Alpha_Nat_array is array (0 .. max_alpha_size) of Natural;

    len          : array (0 .. max_groups) of Alpha_Nat_array;
    limit_glob,  --  Not global actually, but less local than "limit" below
    base_glob,   --  Not global actually, but less local than "base" below
    perm_glob    --  Not global actually, but less local than "perm" below
                 : array (0 .. max_groups) of Alpha_U32_array;
    --
    minlens : Length_array (0 .. max_groups);
    cftab : array (0 .. 257) of Natural;
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

    procedure Create_Huffman_Decoding_Tables (
       limit, base, perm : in out Alpha_U32_array;
       length            : in     Alpha_Nat_array;
       min_len, max_len  : Natural;
       alpha_size        : Integer
    )
    is
      pp, idx : Integer;
      vec : Unsigned_32;
    begin
      pp := 0;
      for i in min_len .. max_len loop
        for j in 0 .. alpha_size - 1 loop
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
      for i in 0 .. alpha_size - 1 loop
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
      block_size := Natural (b) - Character'Pos ('0');
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
        for symbol in 0 .. alpha_size_glob - 1 loop
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
      minlen, maxlen : Natural;
    begin
      for t in 0 .. group_count - 1 loop
        minlen := 32;
        maxlen := 0;
        for i in 0 .. alpha_size_glob - 1 loop
          if len (t)(i) > maxlen then
            maxlen := len (t)(i);
          end if;
          if len (t)(i) < minlen then
            minlen := len (t)(i);
          end if;
        end loop;
        Create_Huffman_Decoding_Tables (
          limit_glob (t), base_glob (t), perm_glob (t), len (t),
          minlen, maxlen, alpha_size_glob
        );
        minlens (t) := minlen;
      end loop;
    end Make_Huffman_Tables;

    -------------------------
    -- MTF - Move To Front --
    -------------------------

    procedure Receive_MTF_Values is
      --
      mtfa_size : constant := 4096;
      mtfl_size : constant := 16;
      mtfbase : array (0 .. 256 / mtfl_size - 1) of Natural;
      mtfa : array (0 .. mtfa_size - 1) of Natural;
      --
      procedure Init_MTF is
        k : Natural := mtfa_size - 1;
      begin
        for i in reverse 0 .. 256  /  mtfl_size - 1 loop
          for j in reverse 0 .. mtfl_size - 1 loop
            mtfa (k) := i * mtfl_size + j;
            k := k - 1;
          end loop;
          mtfbase (i) := k + 1;
        end loop;
      end Init_MTF;
      --
      group_pos, group_no : Integer;
      gminlen, gsel : Natural;
      --
      function Get_MTF_Value return Unsigned_32 is
        zn : Natural;
        zvec : Unsigned_32;
      begin
        if group_pos = 0 then
          group_no := group_no + 1;
          group_pos := group_size;
          gsel := Natural (selector (group_no));
          gminlen := minlens (gsel);
        end if;
        group_pos := group_pos - 1;
        zn := gminlen;
        zvec := Get_Bits_32 (zn);
        while zvec > limit_glob (gsel)(zn) loop
          zn := zn + 1;
          zvec := Shift_Left (zvec, 1) or Get_Bits_32 (1);
        end loop;
        return perm_glob (gsel)(Natural (zvec - base_glob (gsel)(zn)));
      end Get_MTF_Value;
      --
      procedure Move_MTF_Block is
        j, k : Natural;
      begin
        k := mtfa_size;
        for i in reverse 0 .. 256  /  mtfl_size - 1 loop
          j := mtfbase (i);
          mtfa (k - 16 .. k - 1) := mtfa (j .. j + 15);
          k := k - 16;
          mtfbase (i) := k;
        end loop;
      end Move_MTF_Block;
      --
      run_b : constant := 1;
      t : Natural;
      next_sym : Unsigned_32;
      es : Natural;
      n, nn : Natural;
      p, q : Natural;  --  indexes mtfa
      u, v : Natural;  --  indexes mtfbase
      lno, off : Natural;
    begin  --  Receive_MTF_Values
      group_no := -1;
      group_pos := 0;
      t := 0;
      cftab := (others => 0);
      Init_MTF;
      next_sym := Get_MTF_Value;
      --
      while Natural (next_sym) /= inuse_count + 1 loop
        if next_sym <= run_b then
          es := 0;
          n := 0;
          loop
            es := es + Natural (Shift_Left (next_sym + 1, n));
            n := n + 1;
            next_sym := Get_MTF_Value;
            exit when next_sym > run_b;
          end loop;
          n := seq_to_unseq (mtfa (mtfbase (0)));
          cftab (n) := cftab (n) + es;
          if t + es > sub_block_size * block_size then
            raise data_error;
          end if;
          while es > 0 loop
            tt (t) := Unsigned_32 (n);
            es := es - 1;
            t := t + 1;
          end loop;
        else
          nn := Natural (next_sym - 1);
          if nn < mtfl_size then
            --  Avoid the costs of the general case.
            p := mtfbase (0);
            q := p + nn;
            n := mtfa (q);
            loop
              mtfa (q) := mtfa (q - 1);
              q := q - 1;
              exit when q = p;
            end loop;
            mtfa (q) := n;
          else
            --  General case.
            lno := nn   /   mtfl_size;
            off := nn  mod  mtfl_size;
            p := mtfbase (lno);
            q := p + off;
            n := mtfa (q);
            while q /= p loop
              mtfa (q) := mtfa (q - 1);
              q := q - 1;
            end loop;
            u := mtfbase'First;
            v := u + lno;
            loop
              mtfa (mtfbase (v)) := mtfa (mtfbase (v - 1) + mtfl_size - 1);
              v := v - 1;
              mtfbase (v) := mtfbase (v) - 1;
              exit when v = u;
            end loop;
            mtfa (mtfbase (v)) := n;
            if mtfbase (v) = 0 then
              Move_MTF_Block;
            end if;
          end if;
          cftab (seq_to_unseq (n)) := cftab (seq_to_unseq (n)) + 1;
          tt (t) := Unsigned_32 (seq_to_unseq (n));
          t := t + 1;
          if t > sub_block_size * block_size then
            raise data_error;
          end if;
          next_sym := Get_MTF_Value;
        end if;
      end loop;
      tt_count := t;
      --  Setup cftab to facilitate generation of T^(-1).
      t := 0;
      for i in 0 .. 256 loop
        nn := cftab (i);
        cftab (i) := t;
        t := t + nn;
      end loop;
    end Receive_MTF_Values;

    procedure BWT_Detransform is
      a : Unsigned_32 := 0;
      r, i255 : Natural;
    begin
      for p in 0 .. tt_count - 1 loop
        i255 := Natural (tt (p) and 16#ff#);
        r := cftab (i255);
        cftab (i255) := cftab (i255) + 1;
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
        block_origin := Natural (Get_Cardinal_24);
        Receive_Mapping_Table;
        alpha_size_glob := inuse_count + 2;
        Receive_Selectors;
        Undo_MTF_Values_For_Selectors;
        Receive_Huffman_Bit_Lengths;
        Make_Huffman_Tables;
        Receive_MTF_Values;
        --  Undo the Burrows Wheeler transformation.
        BWT_Detransform;
        decode_available := tt_count;
        return True;
      elsif magic = Character'Val (16#17#) & "rE8P" & Character'Val (16#90#) then
        return False;
      else
        raise bad_block_magic;
      end if;
    end Decode_Block;

    next_rle_idx : Integer := -2;
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
          next_rle_idx := Integer (Shift_Right (tt (next_rle_idx), 8));
          decode_available := decode_available - 1;
          if decode_available = 0 then
            compare_final_CRC := True;
            mem_stored_blockcrc := stored_blockcrc;
            --  ^ There might be a new block when last block's
            --    rle is finally emptied.
            --
            --  ** New block
            if Decode_Block then
              next_rle_idx := Natural (Shift_Right (tt (block_origin), 8));
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
          --  The big loop
          loop
            if decode_available = 0 or end_reached then
              exit;
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
            exit when count = 0;
          end loop;
        exception
          when input_dried => RLE_Write;
        end;
        shorten := count;
        rle_run_data := data;
        rle_run_left := rle_len;
      end RLE_Read;

    begin  --  Read
      last := buf'Last;
      if decode_available = Natural'Last then
        --  Initialize the rle process:
        --       - Decode a block
        --       - Initialize pointer.
        if Decode_Block then
          next_rle_idx := Natural (Shift_Right (tt (block_origin), 8));
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
