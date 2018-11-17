--  Legal licensing note:

--  Copyright (c) 2009 .. 2018 Gautier de Montmollin (maintainer of the Ada version)
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

    max_groups    : constant:= 6;
    max_alpha_size: constant:= 258;
    max_code_len  : constant:= 23;
    group_size    : constant:= 50;
    max_selectors : constant:= 2 + (900_000 / group_size);

    sub_block_size: constant:= 100_000;

    type Length_array is array (Integer range <>) of Natural;

    block_randomized: Boolean:= False;
    block_size: Natural;

    use Interfaces;

    type Tcardinal_array is array (Integer range <>) of Unsigned_32;
    type Pcardinal_array is access Tcardinal_array;
    procedure Dispose is new Ada.Unchecked_Deallocation(Tcardinal_array, Pcardinal_array);
    tt: Pcardinal_array;
    tt_count: Natural;

    rle_run_left: Natural:= 0;
    rle_run_data: Unsigned_8:= 0;
    decode_available: Natural:= Natural'Last;
    block_origin: Natural:= 0;
    read_data: Unsigned_8:= 0;
    bits_available: Natural:= 0;
    inuse_count: Natural;
    seq_to_unseq: array (0 .. 255 ) of Natural;
    alpha_size: Natural;
    group_count: Natural;
    --
    selector_count: Natural;
    selector, selector_mtf: array (0 .. max_selectors) of Unsigned_8;
    --
    type Alpha_U32_array is array (0 .. max_alpha_size) of Unsigned_32;
    type Alpha_Nat_array is array (0 .. max_alpha_size) of Natural;

    len  : array (0 .. max_groups) of Alpha_Nat_array;
    limit,
    base ,
    perm : array (0 .. max_groups) of Alpha_U32_array;
    --
    minlens: Length_array(0 .. max_groups);
    cftab: array (0 .. 257) of Natural;
    --
    end_reached: Boolean:= False;

    in_buf: Buffer(1 .. input_buffer_size);
    in_idx: Natural:= in_buf'Last + 1;

    function Read_byte return Unsigned_8 is
      res: Unsigned_8;
    begin
      if in_idx > in_buf'Last then
        Read(in_buf);
        in_idx:= in_buf'First;
      end if;
      res:= in_buf(in_idx);
      in_idx:= in_idx + 1;
      return res;
    end Read_byte;

    procedure hb_create_decode_tables(
       limit, base, perm: in out Alpha_U32_array;
       length           : in     Alpha_Nat_array;
       min_len, max_len : Natural;
       alpha_size       : Integer
    )
    is
      pp, idx: Integer;
      vec: Unsigned_32;
    begin
      pp:=0;
      for i in min_len .. max_len loop
        for j in 0 .. alpha_size-1 loop
          if length(j)=i then
            perm(pp):= Unsigned_32(j);
            pp:= pp + 1;
          end if;
        end loop;
      end loop;
      for i in 0 .. max_code_len-1 loop
        base(i):=0;
        limit(i):=0;
      end loop;
      for i in 0 .. alpha_size-1 loop
        idx:= length(i)+1;
        base(idx):= base(idx) + 1;
      end loop;
      for i in 1 .. max_code_len-1 loop
        base(i):= base(i) + base(i-1);
      end loop;
      vec:=0;
      for i in min_len .. max_len loop
          vec:= vec + base(i+1)-base(i);
          limit(i):= vec-1;
          vec:= vec * 2;
      end loop;
      for i in min_len+1 .. max_len loop
        base(i):=(limit(i-1)+1) * 2 - base(i);
      end loop;
    end hb_create_decode_tables;

    procedure Init is
      magic: String(1..3);
      b: Unsigned_8;
    begin
      --  Read the magic.
      for i in magic'Range loop
        b:= Read_byte;
        magic(i):= Character'Val(b);
      end loop;
      if magic /= "BZh" then
        raise bad_header_magic;
      end if;
      --  Read the block size and allocate the working array.
      b:= Read_byte;
      block_size:= Natural(b) - Character'Pos('0');
      tt:= new Tcardinal_array(0 .. block_size * sub_block_size);
    end Init;

    function get_bits(n: Natural) return Unsigned_8 is
      Result_get_bits : Unsigned_8;
      data: Unsigned_8;
    begin
      if n > bits_available then
        data:= Read_byte;
        Result_get_bits:= Shift_Right(read_data, 8-n) or Shift_Right(data, 8-(n-bits_available));
        read_data:= Shift_Left(data, n-bits_available);
        bits_available:= bits_available + 8;
      else
        Result_get_bits:= Shift_Right(read_data, 8-n);
        read_data:= Shift_Left(read_data, n);
      end if;
      bits_available:= bits_available - n;
      return Result_get_bits;
    end get_bits;

    function get_bits_32(n: Natural) return Unsigned_32 is
    begin
      return Unsigned_32(get_bits(n));
    end get_bits_32;

    function get_boolean return Boolean is
    begin
      return Boolean'Val(get_bits(1));
    end get_boolean;

    function get_byte return Unsigned_8 is
    begin
      return get_bits(8);
    end get_byte;

    function get_cardinal24 return Unsigned_32 is
    begin
      return Shift_Left(get_bits_32(8),16) or Shift_Left(get_bits_32(8),8) or get_bits_32(8);
    end get_cardinal24;

    function get_cardinal return Unsigned_32 is
    begin
      return Shift_Left(get_bits_32(8),24)  or
             Shift_Left(get_bits_32(8),16)  or
             Shift_Left(get_bits_32(8), 8)  or
             get_bits_32(8);
    end get_cardinal;

    --  Receive the mapping table. To save space, the inuse set is stored in pieces
    --  of 16 bits. First 16 bits are stored which pieces of 16 bits are used, then
    --  the pieces follow.
    procedure receive_mapping_table is
      inuse16: array(0 .. 15) of Boolean;
      --* inuse: array(0 .. 255) of Boolean; -- for dump purposes
    begin
      --  Receive the first 16 bits which tell which pieces are stored.
      for i in inuse16'Range loop
        inuse16(i) := get_boolean;
      end loop;
      --  Receive the used pieces.
      --* inuse:= (others => False);
      inuse_count := 0;
      for i in inuse16'Range loop
        if inuse16(i) then
          for j in 0 .. 15 loop
            if get_boolean then
              --* inuse(16*i+j):= True;
              seq_to_unseq(inuse_count) := 16*i + j;
              inuse_count:= inuse_count + 1;
            end if;
          end loop;
        end if;
      end loop;
    end receive_mapping_table;

    --  Receives the selectors.
    procedure receive_selectors is
      j: Unsigned_8;
    begin
      group_count:= Natural(get_bits(3));
      selector_count:= Natural(Shift_Left(get_bits_32(8), 7) or get_bits_32(7));
      for i in 0 .. selector_count-1 loop
        j:=0;
        while get_boolean loop
          j:= j + 1;
          if j > 5 then
            raise data_error;
          end if;
        end loop;
        selector_mtf(i):=j;
      end loop;
    end receive_selectors;

    --  Undo the MTF values for the selectors.
    procedure undo_mtf_values is
      pos: array (0 .. max_groups) of Natural;
      v, tmp: Natural;
    begin
      for w in 0 .. group_count-1 loop
        pos(w):=w;
      end loop;
      for i in 0 .. selector_count-1 loop
        v:= Natural(selector_mtf(i));
        tmp:=pos(v);
        while v/=0 loop
          pos(v):= pos(v-1);
          v:= v - 1;
        end loop;
        pos(0):= tmp;
        selector(i):= Unsigned_8(tmp);
      end loop;
    end undo_mtf_values;

    procedure receive_coding_tables is
      curr: Natural;
    begin
      for t in 0 .. group_count-1 loop
        curr:= Natural(get_bits(5));
        for i in 0 .. alpha_size-1 loop
          loop
            if curr not in 1..20 then
              raise data_error;
            end if;
            exit when not get_boolean;
            if get_boolean then
              curr:= curr - 1;
            else
              curr:= curr + 1;
            end if;
          end loop;
          len(t)(i):=curr;
        end loop;
      end loop;
    end receive_coding_tables;

    --  Builds the Huffman tables.
    procedure make_hufftab is
      minlen, maxlen: Natural;
    begin
      for t in 0 .. group_count-1 loop
        minlen:= 32;
        maxlen:= 0;
        for i in 0 .. alpha_size-1 loop
          if len(t)(i) > maxlen then
            maxlen:= len(t)(i);
          end if;
          if len(t)(i) < minlen then
            minlen:= len(t)(i);
          end if;
        end loop;
        hb_create_decode_tables(
          limit(t), base(t), perm(t), len(t),
          minlen, maxlen, alpha_size
        );
        minlens(t):= minlen;
      end loop;
    end make_hufftab;

    -------------------------
    -- MTF - Move To Front --
    -------------------------

    procedure receive_mtf_values is
      --
      mtfa_size: constant:= 4096;
      mtfl_size: constant:= 16;
      mtfbase: array (0 .. 256 / mtfl_size-1) of Natural;
      mtfa: array (0 .. mtfa_size-1) of Natural;
      --
      procedure init_mtf is
        k: Natural:= mtfa_size-1;
      begin
        for i in reverse 0 .. 256  /  mtfl_size-1 loop
          for j in reverse 0 .. mtfl_size-1 loop
            mtfa(k):= i*mtfl_size + j;
            k:= k - 1;
          end loop;
          mtfbase(i):= k+1;
        end loop;
      end init_mtf;
      --
      group_pos, group_no: Integer;
      gminlen, gsel: Natural;
      --
      function get_mtf_value return Unsigned_32 is
        zn: Natural;
        zvec: Unsigned_32;
      begin
        if group_pos = 0 then
          group_no:= group_no + 1;
          group_pos:= group_size;
          gsel:= Natural(selector(group_no));
          gminlen:= minlens(gsel);
        end if;
        group_pos:= group_pos - 1;
        zn:= gminlen;
        zvec:= get_bits_32(zn);
        while zvec > limit(gsel)(zn) loop
          zn:= zn + 1;
          zvec:= Shift_Left(zvec, 1) or get_bits_32(1);
        end loop;
        return perm(gsel)(Natural(zvec-base(gsel)(zn)));
      end get_mtf_value;
      --
      procedure move_mtf_block is
        j, k: Natural;
      begin
        k:= mtfa_size;
        for i in reverse 0 .. 256  /  mtfl_size-1 loop
          j:= mtfbase(i);
          mtfa(k-16..k-1):= mtfa(j..j+15);
          k:= k - 16;
          mtfbase(i):= k;
        end loop;
      end move_mtf_block;
      --
      run_b: constant:= 1;
      t: Natural;
      next_sym: Unsigned_32;
      es: Natural;
      n, nn: Natural;
      p,q: Natural; -- indexes mtfa
      u,v: Natural; -- indexes mtfbase
      lno, off: Natural;
    begin -- receive_mtf_values
      group_no:= -1;
      group_pos:= 0;
      t:= 0;
      cftab:= (others => 0);
      init_mtf;
      next_sym:= get_mtf_value;
      --
      while Natural(next_sym) /= inuse_count+1 loop
        if next_sym <= run_b then
          es:= 0;
          n:= 0;
          loop
            es:= es + Natural(Shift_Left(next_sym+1, n));
            n:= n + 1;
            next_sym:= get_mtf_value;
            exit when next_sym > run_b;
          end loop;
          n:= seq_to_unseq( mtfa(mtfbase(0)) );
          cftab(n):= cftab(n) + es;
          if t+es > sub_block_size * block_size then
            raise data_error;
          end if;
          while es > 0 loop
            tt(t):= Unsigned_32(n);
            es:= es - 1;
            t:= t + 1;
          end loop;
        else
          nn:= Natural(next_sym - 1);
          if nn < mtfl_size then
            -- Avoid the costs of the general case.
            p:= mtfbase(0);
            q:= p + nn;
            n:= mtfa(q);
            loop
              mtfa(q):= mtfa(q-1);
              q:= q - 1;
              exit when q = p;
            end loop;
            mtfa(q):= n;
          else
            --  General case.
            lno:= nn   /   mtfl_size;
            off:= nn  mod  mtfl_size;
            p:= mtfbase(lno);
            q:= p + off;
            n:= mtfa(q);
            while q /= p loop
              mtfa(q):= mtfa(q-1);
              q:= q - 1;
            end loop;
            u:= mtfbase'First;
            v:= u + lno;
            loop
              mtfa(mtfbase(v)):= mtfa(mtfbase(v-1)+mtfl_size-1);
              v:= v - 1;
              mtfbase(v):= mtfbase(v) - 1;
              exit when v = u;
            end loop;
            mtfa( mtfbase(v) ):= n;
            if mtfbase(v) = 0 then
              move_mtf_block;
            end if;
          end if;
          cftab(seq_to_unseq(n)):= cftab(seq_to_unseq(n)) + 1;
          tt(t):= Unsigned_32(seq_to_unseq(n));
          t:= t + 1;
          if t > sub_block_size * block_size then
            raise data_error;
          end if;
          next_sym:= get_mtf_value;
        end if;
      end loop;
      tt_count:= t;
      --  Setup cftab to facilitate generation of T^(-1).
      t:= 0;
      for i in 0 .. 256 loop
        nn:= cftab(i);
        cftab(i):= t;
        t:= t + nn;
      end loop;
    end receive_mtf_values;

    procedure BWT_Detransform is
      a : Unsigned_32 := 0;
      r, i255: Natural;
    begin
      for p in 0 .. tt_count - 1 loop
        i255 := Natural(tt(p) and 16#ff#);
        r := cftab(i255);
        cftab(i255) := cftab(i255) + 1;
        tt(r) := tt(r) or a;
        a := a + 16#100#;
      end loop;
    end BWT_Detransform;

    compare_final_CRC: Boolean:= False;
    stored_blockcrc, mem_stored_blockcrc, computed_crc: Unsigned_32;

    -- Decode a new compressed block.
    function decode_block return Boolean is
      magic: String(1 .. 6);
    begin
      for i in 1 .. 6 loop
        magic(i):= Character'Val(get_byte);
      end loop;
      if magic = "1AY&SY" then
        if check_CRC then
          if compare_final_CRC then
            null; -- initialisation is delayed until the rle buffer is empty
          else
            CRC.Init(computed_crc); -- Initialize for next block.
          end if;
        end if;
        stored_blockcrc:= get_cardinal;
        block_randomized:= get_boolean;
        block_origin:= Natural(get_cardinal24);
        --  Receive the mapping table.
        receive_mapping_table;
        alpha_size:= inuse_count + 2;
        --  Receive the selectors.
        receive_selectors;
        --  Undo the MTF values for the selectors.
        undo_mtf_values;
        --  Receive the coding tables.
        receive_coding_tables;
        --  Build the Huffman tables.
        make_hufftab;
        --  Receive the MTF values.
        receive_mtf_values;
        --  Undo the Burrows Wheeler transformation.
        BWT_Detransform;
        decode_available := tt_count;
        return True;
      elsif magic = Character'Val(16#17#) & "rE8P" & Character'Val(16#90#) then
        return False;
      else
        raise bad_block_magic;
      end if;
    end decode_block;

    next_rle_idx: Integer:= -2;
    buf: Buffer(1 .. output_buffer_size);
    last: Natural;

    procedure Read is
      shorten: Natural:= 0;

      procedure rle_read is
        rle_len: Natural;
        data: Unsigned_8;
        idx: Integer:= buf'First;
        count: Integer:= buf'Length;
        --
        procedure rle_write is
          pragma Inline(rle_write);
        begin
          loop
            buf(idx):= data;
            idx:= idx + 1;
            count:= count - 1;
            rle_len:= rle_len - 1;
            if check_CRC then
              CRC.Update(computed_crc, data);
              if rle_len = 0 and then compare_final_CRC then
                if CRC.Final(computed_crc) /= mem_stored_blockcrc then
                  raise block_crc_check_failed;
                end if;
                compare_final_CRC:= False;
                CRC.Init(computed_crc); -- Initialize for next block.
              end if;
            end if;
            exit when rle_len = 0 or count = 0;
          end loop;
        end rle_write;
        --
        --  Handle extreme cases of data of length 1, 2.
        --  This exception is always handled (see end of rle_read).
        input_dried : exception;
        --
        -- Make next_rle_idx index to the next decoded byte.
        -- If next_rle_idx did index to the last
        -- byte in the current block, decode the next block.
        --
        procedure consume_rle is
          pragma Inline(consume_rle);
        begin
          next_rle_idx:= Integer(Shift_Right(tt(next_rle_idx),8));
          decode_available:= decode_available - 1;
          if decode_available = 0 then
            compare_final_CRC:= True;
            mem_stored_blockcrc:= stored_blockcrc;
            -- ^ There might be a new block when last block's
            --   rle is finally emptied.
            --
            -- ** New block
            if decode_block then
              next_rle_idx:= Natural(Shift_Right(tt(block_origin),8));
            else
              next_rle_idx:= -1;
              end_reached:= True;
            end if;
            -- **
            if end_reached then
              raise input_dried;
            end if;
          end if;
        end consume_rle;
        --
        function rle_byte return Unsigned_8 is
          pragma Inline(rle_byte);
        begin
          return Unsigned_8(tt(next_rle_idx) and 16#FF#);
        end rle_byte;
        --
        function rle_possible return Boolean is
          pragma Inline(rle_possible);
        begin
          return decode_available > 0 and then data = rle_byte;
        end rle_possible;
        --
      begin -- rle_read
        rle_len:= rle_run_left;
        data:= rle_run_data;
        if block_randomized then
          raise randomized_not_yet_implemented;
        end if;
        if rle_len /= 0 then
          rle_write;
          if count = 0 then
            shorten:= 0;
            rle_run_data:= data;
            rle_run_left:= rle_len;
            return;
          end if;
        end if;
        begin
          -- The big loop
          loop
            if decode_available = 0 or end_reached then
              exit;
            end if;
            rle_len:= 1;
            data:= rle_byte;
            consume_rle;
            if rle_possible then
              rle_len:= rle_len + 1;
              consume_rle;
              if rle_possible then
                rle_len:= rle_len + 1;
                consume_rle;
                if rle_possible then
                  consume_rle;
                  rle_len:= rle_len + Natural(rle_byte)+1;
                  consume_rle;
                end if;
              end if;
            end if;
            rle_write;
            exit when count = 0;
          end loop;
        exception
          when input_dried => rle_write;
        end;
        shorten:= count;
        rle_run_data:= data;
        rle_run_left:= rle_len;
      end rle_read;

    begin -- read
      last:= buf'Last;
      if decode_available = Natural'Last then
        --  Initialize the rle process:
        --       - Decode a block
        --       - Initialize pointer.
        if decode_block then
          next_rle_idx:= Natural(Shift_Right(tt(block_origin), 8));
        else
          next_rle_idx:= -1;
          end_reached:= True;
        end if;
      end if;
      rle_read;
      last:= last - shorten;
    end Read;

  begin
    Init;
    loop
      Read;
      Write( buf(1..last) );
      exit when end_reached and rle_run_left = 0;
    end loop;
    Dispose(tt);
  end Decompress;

end BZip2.Decoding;
