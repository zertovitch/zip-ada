--  There are four LZ77 encoders at choice in this package:
--
--    1/  LZ77_using_LZHuf, based on LZHuf by Haruhiko Okumura and Haruyasu Yoshizaki.
--
--    2/  LZ77_using_IZ, based on Info-Zip's Zip's deflate.c by Jean-Loup Gailly.
--          deflate.c is actually the LZ77 part of Info-Zip's compression.
--
--    3/  LZ77_using_BT4, based on LZMA SDK's BT4 algorithm by Igor Pavlov.
--
--    4/  LZ77_by_Rich, based on PROG2.C by Rich Geldreich, Jr.
--
--  Variant 1/, LZ77_using_LZHuf, is working since 2009. Two problems: it is slow
--     and not well adapted to the Deflate format (mediocre compression).
--
--  Variant 2/, LZ77_using_IZ, is much faster, and better suited for Deflate.
--     It has been added on 05-Mar-2016.
--     The code is tailored and optimized for a single set of
--     the String_buffer_size, Look_Ahead, Threshold LZ77 parameters - those for Deflate.
--
--  Variant 3/, LZ77_using_BT4, was added on 06-Sep-2016.
--     The seems to be the best match finder for LZMA on data of the >= 1 MiB scale.

--  To do:
--
--  2/
--    - LZ77 / IZ: similar to the test with TOO_FAR, try to cluster distances around
--        values needing less extra bits (may not work at all...)
--    - LZ77 / IZ: tune TOO_FAR (max: 32767), see http://optipng.sf.net/pngtech/too_far.html
--        "TOO_FAR in zlib Is Not Too Far" for discussion

--  Legal licensing note:

--  Copyright (c) 2016 .. 2019 Gautier de Montmollin (maintainer of the Ada version)
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

with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System;

package body LZ77 is

  --  System.Word_Size: 13.3(8): A word is the largest amount of storage
  --  that can be conveniently and efficiently manipulated by the hardware,
  --  given the implementation's run-time model.
  --
  min_bits_32 : constant := Integer'Max (32, System.Word_Size);
  min_bits_16 : constant := Integer'Max (16, System.Word_Size);

  --  We define an Integer type which is at least 32 bits, but n bits
  --  on a native n (> 32) bits architecture (no performance hit on 64+
  --  bits architectures).
  --  Integer_M16 not needed: Integer already guarantees 16 bits
  --
  type Integer_M32 is range -2**(min_bits_32 - 1) .. 2**(min_bits_32 - 1) - 1;
  subtype Natural_M32  is Integer_M32 range 0 .. Integer_M32'Last;

  type Unsigned_M16 is mod 2**min_bits_16;
  type Unsigned_M32 is mod 2**min_bits_32;

  procedure Encode is

    -----------------------
    --  LZHuf algorithm  --
    -----------------------

    procedure LZ77_using_LZHuf is
      --  Based on LZHUF by OKUMURA & YOSHIZAKI.
      --  Here the adaptive Huffman coding is thrown away:
      --  algorithm is used only to find matching patterns.

      N_Char    : constant Integer := 256 - Threshold + Look_Ahead;
      --  Character code (= 0..N_CHAR-1)
      Max_Table     : constant Integer := N_Char * 2 - 1;

      type Text_Buffer is array (0 .. String_buffer_size + Look_Ahead - 1) of Byte;
      empty_buffer : constant Text_Buffer := (others => 32);  --  ' '

      --  > The Huffman frequency handling is made generic so we have
      --    one copy of the tree and of the frequency table for Encode
      --    and one for Decode

      generic
      package Huffman is
        --- Pointing parent nodes.
        --- Area [Max_Table..(Max_Table + N_CHAR - 1)] are pointers for leaves
        Parent :  array (0 .. Max_Table + N_Char - 1) of Natural;
        --- Pointing children nodes (son[], son[] + 1)
        Son    :  array (0 .. Max_Table - 1)  of Natural;

        Root_Position : constant Integer := Max_Table - 1;  --  (can be always Son'last ?)

        procedure Start;
        procedure Update_Freq_Tree (C0 : Natural);
      end Huffman;

      package body Huffman is

        Freq : array (0 .. Max_Table) of Natural;  --  Cumulative freq table

        Max_Freq : constant := 16#8000#;
        --  ^-- update when cumulative frequency reaches to this value

        procedure Start is
          I : Natural;
        begin
          for J in  0 .. N_Char - 1  loop
            Freq (J) := 1;
            Son (J) := J + Max_Table;
            Parent (J + Max_Table) := J;
          end loop;

          I := 0;
          for J in N_Char .. Root_Position  loop
            Freq (J) := Freq (I) + Freq (I + 1);
            Son (J) := I;
            Parent (I) := J;
            Parent (I + 1) := J;
            I := I + 2;
          end loop;

          Freq (Freq'Last) := 16#FFFF#;  --  ( Max_Table )
          Parent (Root_Position) := 0;
        end Start;

        procedure Update_Freq_Tree (C0 : Natural) is

          procedure Reconstruct_Freq_Tree is
            I, J, K, F, L : Natural;
          begin
            --  Halven cumulative freq for leaf nodes
            J := 0;
            for I in 0 .. Root_Position loop
              if Son (I) >= Max_Table then
                Freq (J) := (Freq (I) + 1) / 2;
                Son (J) := Son (I);
                J := J + 1;
              end if;
            end loop;

            --  Make a tree : first, connect children nodes
            I := 0;
            for J in N_Char .. Root_Position loop  --  J : free nodes
              K := I + 1;
              F := Freq (I) + Freq (K); -- new frequency
              Freq (J) := F;
              K := J - 1;
              while F < Freq (K) loop
                K := K - 1;
              end loop;

              K := K + 1;
              L := J - K;  --  2007: fix: was L:= (J-K)*2, memcopy parameter remain

              Freq (K + 1 .. K + L) := Freq (K .. K + L - 1);  --  shift by one cell right
              Freq (K) := F;
              Son (K + 1 .. K + L) := Son (K .. K + L - 1);  --  shift by one cell right
              Son (K) := I;
              I := I + 2;
            end loop;

            --  Connect parent nodes
            for I in 0 .. Max_Table - 1 loop
              K := Son (I);
              Parent (K) := I;
              if K < Max_Table then
                Parent (K + 1) := I;
              end if;
            end loop;

          end Reconstruct_Freq_Tree;

          C, I, J, K, L : Natural;

        begin  --  Update_Freq_Tree;
          if Freq (Root_Position) = Max_Freq then
            Reconstruct_Freq_Tree;
          end if;
          C := Parent (C0 + Max_Table);
          loop
            Freq (C) := Freq (C) + 1;
            K := Freq (C);
            --  Swap nodes to keep the tree freq-ordered
            L := C + 1;
            if  K > Freq (L) then
              while K > Freq (L + 1) loop
                L := L + 1;
              end loop;

              Freq (C) := Freq (L);
              Freq (L) := K;

              I := Son (C);
              Parent (I) := L;
              if I < Max_Table then
                Parent (I + 1) := L;
              end if;

              J := Son (L);
              Son (L) := I;

              Parent (J) := C;
              if J < Max_Table then
                Parent (J + 1) := C;
              end if;
              Son (C) := J;

              C := L;
            end if;
            C := Parent (C);
            exit when C = 0;
          end loop;        -- do it until reaching the root
        end Update_Freq_Tree;

      end Huffman;

      Node_Nil : constant Integer := String_buffer_size;    --  End of tree's node

      Lson, Dad :  array (0 .. String_buffer_size) of Natural;
      Rson :       array (0 .. String_buffer_size + 256) of Natural;

      procedure Init_Tree is
      begin
        for I in String_buffer_size + 1 .. Rson'Last loop
          Rson (I) := Node_Nil;
        end loop;  --  root
        for I in 0 .. String_buffer_size - 1 loop
          Dad (I) := Node_Nil;
        end loop;  --  node
      end Init_Tree;

      Match_Position : Natural;
      Match_Length   : Natural;

      Text_Buf : Text_Buffer := empty_buffer;

      procedure Insert_Node (R : Integer) is
        I, P : Integer;
        Geq  : Boolean := True;
        C    : Natural;
      begin
        P := String_buffer_size + 1 + Integer (Text_Buf (R));
        Rson (R) := Node_Nil;
        Lson (R) := Node_Nil;
        Match_Length := 0;
        loop
          if Geq then
            if Rson (P) = Node_Nil then
              Rson (P) := R;
              Dad (R) := P;
              return;
            end if;
            P := Rson (P);
          else
            if Lson (P) = Node_Nil then
              Lson (P) := R;
              Dad (R) := P;
              return;
            end if;
            P := Lson (P);
          end if;
          I := 1;
          while I < Look_Ahead and then Text_Buf (R + I) = Text_Buf (P + I)  loop
            I := I + 1;
          end loop;

          Geq := Text_Buf (R + I) >= Text_Buf (P + I) or I = Look_Ahead;

          if I > Threshold then
            if I > Match_Length then
              Match_Position := (R - P) mod String_buffer_size - 1;
              Match_Length := I;
              exit when Match_Length >= Look_Ahead;
            end if;
            if I = Match_Length then
              C := (R - P) mod String_buffer_size - 1;
              if C < Match_Position then
                Match_Position := C;
              end if;
            end if;
          end if;
        end loop;

        Dad (R)  := Dad (P);
        Lson (R) := Lson (P);
        Rson (R) := Rson (P);
        Dad (Lson (P)) := R;
        Dad (Rson (P)) := R;
        if Rson (Dad (P)) = P then
          Rson (Dad (P)) := R;
        else
          Lson (Dad (P)) := R;
        end if;
        Dad (P) := Node_Nil;  --  remove P
      end Insert_Node;

      procedure Delete_Node (P : Natural) is
        Q : Natural;
      begin
        if Dad (P) = Node_Nil then  --  unregistered
          return;
        end if;
        if Rson (P) = Node_Nil then
          Q := Lson (P);
        elsif Lson (P) = Node_Nil then
          Q := Rson (P);
        else
          Q := Lson (P);
          if Rson (Q) /= Node_Nil then
            loop
              Q := Rson (Q);
              exit when Rson (Q) = Node_Nil;
            end loop;
            Rson (Dad (Q)) := Lson (Q);
            Dad (Lson (Q)) := Dad (Q);
            Lson (Q) := Lson (P);
            Dad (Lson (P)) := Q;
          end if;
          Rson (Q) := Rson (P);
          Dad (Rson (P)) := Q;
        end if;
        Dad (Q) := Dad (P);
        if Rson (Dad (P)) = P then
          Rson (Dad (P)) := Q;
        else
          Lson (Dad (P)) := Q;
        end if;
        Dad (P) := Node_Nil;
      end Delete_Node;

      package Huffman_E is new Huffman;

      I, R, S, Last_Match_Length : Natural;
      Len : Integer;
      C : Byte;
    begin
      if not More_bytes then
        return;
      end if;
      Huffman_E.Start;
      Init_Tree;
      S := 0;
      R := String_buffer_size - Look_Ahead;
      Len := 0;
      while Len < Look_Ahead and More_bytes loop
        Text_Buf (R + Len) := Read_byte;
        Len := Len + 1;
      end loop;

      --  Seems: fill dictionary with default value
      --
      --  for I in 1.. Look_Ahead loop
      --    Insert_Node(R - I);
      --  end loop;

      Insert_Node (R);

      loop
        if Match_Length > Len then
          Match_Length := Len;
        end if;
        if Match_Length <= Threshold then
          Match_Length := 1;
          Huffman_E.Update_Freq_Tree (Natural (Text_Buf (R)));
          Write_literal (Text_Buf (R));
        else
          Write_DL_code (Match_Position + 1, Match_Length);
        end if;
        Last_Match_Length := Match_Length;
        I := 0;
        while I < Last_Match_Length and More_bytes loop
          I := I + 1;
          Delete_Node (S);
          C := Read_byte;
          Text_Buf (S) := C;
          if  S < Look_Ahead - 1 then
            Text_Buf (S + String_buffer_size) := C;
          end if;
          S := (S + 1) mod String_buffer_size;
          R := (R + 1) mod String_buffer_size;
          Insert_Node (R);
        end loop;

        while I < Last_Match_Length loop
          I := I + 1;
          Delete_Node (S);
          S := (S + 1) mod String_buffer_size;
          R := (R + 1) mod String_buffer_size;
          Len := Len - 1;
          if Len > 0 then
            Insert_Node (R);
          end if;
        end loop;

        exit when Len = 0;
      end loop;
    end LZ77_using_LZHuf;

    --------------------------
    --  Info-Zip algorithm  --
    --------------------------

    --  LZ77_using_IZ: based on deflate.c by Jean-Loup Gailly.
    --  Core description of the algorithm:
    --
    --     The most straightforward technique turns out to be the fastest for
    --     most input files: try all possible matches and select the longest.
    --     The key feature of this algorithm is that insertions into the string
    --     dictionary are very simple and thus fast, and deletions are avoided
    --     completely. Insertions are performed at each input character, whereas
    --     string matches are performed only when the previous match ends. So it
    --     is preferable to spend more time in matches to allow very fast string
    --     insertions and avoid deletions. The matching algorithm for small
    --     strings is inspired from that of Rabin & Karp [1]. A brute force approach
    --     is used to find longer strings when a small match has been found.
    --
    --     The idea of lazy evaluation of matches is due to Jan-Mark Wams.
    --
    --     [1] A description of the Rabin and Karp algorithm is given in the book
    --         "Algorithms" by R. Sedgewick, Addison-Wesley, p252.
    --
    --  About hashing: chapter 6.4 of The Art of Computer Programming, Volume 3, D.E. Knuth
    --  Rabin and Karp algorithm: http://en.wikipedia.org/wiki/Rabin%E2%80%93Karp_algorithm

    --  Compression level: 0: store, 1: best speed, 9: best compression, 10: variant of level 9
    --  Ada code: only levels 4 to 10 are supported.

    procedure LZ77_using_IZ (level : Natural) is
      HASH_BITS : constant := 15;  --  13..15
      HASH_SIZE : constant := 2 ** HASH_BITS;
      HASH_MASK : constant := HASH_SIZE - 1;
      WSIZE     : constant Integer_M32 := Integer_M32 (String_buffer_size);
      WMASK     : constant Unsigned_M16 := Unsigned_M16 (WSIZE - 1);
      --  HASH_SIZE and WSIZE must be powers of two
      NIL      : constant := 0;     --  Tail of hash chains
      TOO_FAR  : constant := 4096;  --  Matches of length 3 are discarded if their distance exceeds TOO_FAR
      --
      subtype ulg is Unsigned_M32;
      subtype unsigned is Unsigned_M16;
      subtype ush is Unsigned_M16;
      --  subtype long is Integer_M32;
      --  subtype int is Integer;
      subtype Pos is Unsigned_M32;  --  must be at least 32 bits
      --  subtype IPos is unsigned;
      --  A Pos is an index in the character window. IPos is used only for parameter passing.
      window : array (0 .. 2 * WSIZE - 1) of Byte;
      --  Sliding window. Input bytes are read into the second half of the window,
      --  and move to the first half later to keep a dictionary of at least WSIZE
      --  bytes. With this organization, matches are limited to a distance of
      --  WSIZE-MAX_MATCH bytes, but this ensures that IO is always
      --  performed with a length multiple of the block size.
      prev : array (0 .. unsigned (WSIZE - 1)) of Pos;
      --  Link to older string with same hash index.
      --  This link is maintained only for the last 32K strings.
      --  An index in this array is thus a window index modulo 32K.
      head : array (0 .. unsigned (HASH_SIZE - 1)) of Pos;
      --  Heads of the hash chains or NIL.
      window_size : ulg;
      --  window size, 2*WSIZE except for MMAP or BIG_MEM, where it is the
      --  input file length plus MIN_LOOKAHEAD.
      sliding : Boolean;  --  Set to False when the input file is already in memory  [was: int]
      ins_h : unsigned;   --  hash index of string to be inserted
      MIN_MATCH : constant Integer_M32 := Integer_M32 (Threshold) + 1;    --  Deflate: 3
      MAX_MATCH : constant Integer_M32 := Integer_M32 (Look_Ahead);       --  Deflate: 258
      --  Minimum amount of lookahead, except at the end of the input file.
      MIN_LOOKAHEAD : constant Integer_M32 := MAX_MATCH + MIN_MATCH + 1;  --  Deflate: 262
      --  This LZ77 compression doesn't use the full possible distance range: 32507..32768 unused!
      MAX_DIST : constant Integer_M32 := WSIZE - MIN_LOOKAHEAD;  --  Deflate: 32506
      H_SHIFT : constant Integer := Integer ((HASH_BITS + MIN_MATCH - 1) / MIN_MATCH);
      --  Number of bits by which ins_h and del_h must be shifted at each
      --  input step. It must be such that after MIN_MATCH steps, the oldest
      --  byte no longer takes part in the hash key, that is:
      --  H_SHIFT * MIN_MATCH >= HASH_BITS
      prev_length : Natural_M32; --  [was: unsigned]
      --  Length of the best match at previous step. Matches not greater than this
      --  are discarded. This is used in the lazy match evaluation.
      strstart    : Natural_M32;   --  start of string to insert [was: unsigned]
      match_start : Natural_M32;   --  start of matching string [was: unsigned]
      eofile      : Boolean;       --  flag set at end of input file [was: int]
      lookahead   : Natural_M32;   --  number of valid bytes ahead in window  [was: unsigned]
      max_chain_length : unsigned;
      --  To speed up deflation, hash chains are never searched beyond this length.
      --  A higher limit improves compression ratio but degrades the speed.
      max_lazy_match : Natural_M32;  --  [was: unsigned]
      --  Attempt to find a better match only when the current match is strictly
      --  smaller than this value. This mechanism is used only for compression
      --  levels >= 4.
      good_match : Natural_M32;  --  [was: unsigned]
      --  Use a faster search when the previous match is longer than this
      nice_match : Integer_M32;  --  Stop searching when current match exceeds this
      --  Values for max_lazy_match, good_match, nice_match and max_chain_length,
      --  depending on the desired pack level (0..9). The values given below have
      --  been tuned to exclude worst case performance for pathological files.
      --  Better values may be found for specific files.
      type config is record
        good_length  : Natural_M32;  --  reduce lazy search above this match length [was: ush]
        max_lazy     : Natural_M32;  --  do not perform lazy search above this match length
        nice_length  : Integer_M32;  --  quit search above this match length
        max_chain    : ush;
      end record;

      configuration_table : constant array (0 .. 10) of config := (
      --  good lazy nice chain
          (0,    0,  0,    0),    --  0: store only
          (4,    4,  8,    4),    --  1: maximum speed, no lazy matches
          (4,    5, 16,    8),
          (4,    6, 32,   32),
          (4,    4, 16,   16),    --  4: lazy matches
          (8,   16, 32,   32),
          (8,   16, 128, 128),
          (8,   32, 128, 256),
          (32, 128, 258, 1024),
          (32, 258, 258, 4096),   --  9: maximum compression
          (34, 258, 258, 4096));  --  "secret" variant of level 9

      --  Update a hash value with the given input byte
      --  IN  assertion: all calls to to UPDATE_HASH are made with consecutive
      --     input characters, so that a running hash key can be computed from the
      --     previous key instead of complete recalculation each time.

      procedure UPDATE_HASH (h : in out unsigned; c : Byte) is
      pragma Inline (UPDATE_HASH);
      begin
        h := (unsigned (Shift_Left (Unsigned_32 (h), H_SHIFT)) xor unsigned (c)) and HASH_MASK;
      end UPDATE_HASH;

      --  Insert string starting at s in the dictionary and set match_head to the previous head
      --  of the hash chain (the most recent string with same hash key). Return
      --  the previous length of the hash chain.
      --  IN  assertion: all calls to to INSERT_STRING are made with consecutive
      --     input characters and the first MIN_MATCH bytes of s are valid
      --     (except for the last MIN_MATCH-1 bytes of the input file).

      procedure INSERT_STRING (s : Integer_M32; match_head : out Natural_M32) is
      pragma Inline (INSERT_STRING);
      begin
        UPDATE_HASH (ins_h, window (s + MIN_MATCH - 1));
        match_head := Natural_M32 (head (ins_h));
        prev (unsigned (s) and WMASK) := Pos (match_head);
        head (ins_h) := Pos (s);
      end INSERT_STRING;

      procedure Read_buf (from : Integer_M32; amount : unsigned; actual : out Integer_M32) is
        need : unsigned := amount;
      begin
        --  put_line("Read buffer: from:" & from'img & ";  amount:" & amount'img);
        actual := 0;
        while need > 0 and then More_bytes loop
          window (from + actual) := Read_byte;
          actual := actual + 1;
          need := need - 1;
        end loop;
        --  put_line("Read buffer: actual:" & actual'img);
      end Read_buf;

      --  Fill the window when the lookahead becomes insufficient.
      --  Updates strstart and lookahead, and sets eofile if end of input file.
      --
      --  IN assertion: lookahead < MIN_LOOKAHEAD && strstart + lookahead > 0
      --  OUT assertions: strstart <= window_size-MIN_LOOKAHEAD
      --     At least one byte has been read, or eofile is set; file reads are
      --     performed for at least two bytes (required for the translate_eol option).

      procedure Fill_window is
        more : unsigned;
        m : Pos;
        n : Natural_M32;
      begin
        loop
          more := unsigned (window_size - ulg (lookahead) - ulg (strstart));
          if False then  --  C: "if (more == (unsigned)EOF) {" ?... GdM: seems a 16-bit code for EOF
            --  Very unlikely, but possible on 16 bit machine if strstart == 0
            --  and lookahead == 1 (input done one byte at time)
            more := more - 1;
          elsif strstart >= WSIZE + MAX_DIST and then sliding then
            --  By the IN assertion, the window is not empty so we can't confuse
            --  more == 0 with more == 64K on a 16 bit machine.
            window (0 .. WSIZE - 1) := window (WSIZE .. 2 * WSIZE - 1);
            --  GdM: in rare cases (e.g. level 9 on test file "enwik8"), match_start happens
            --  to be < WSIZE. We do as in the original 16-bit C code: mod 2**16, such that the
            --  index is the window's range.
            --  This assumes WSIZE = 2**15, which is checked at startup of LZ77_using_IZ.
            --  Very likely, match_start is garbage anyway - see http://sf.net/p/infozip/bugs/49/
            match_start := Natural_M32 (Unsigned_16 (match_start) - Unsigned_16 (WSIZE mod (2**16)));
            strstart    := strstart - WSIZE; -- we now have strstart >= MAX_DIST:
            for nn in 0 .. unsigned'(HASH_SIZE - 1) loop
              m := head (nn);
              if m >= Pos (WSIZE) then
                head (nn) := m - Pos (WSIZE);
              else
                head (nn) := NIL;
              end if;
            end loop;
            --
            for nn in 0 .. unsigned (WSIZE - 1) loop
              m := prev (nn);
              if m >= Pos (WSIZE) then
                prev (nn) := m - Pos (WSIZE);
              else
                prev (nn) := NIL;
              end if;
              --  If n is not on any hash chain, prev[n] is garbage but its value will never be used.
            end loop;
            more := more + unsigned (WSIZE);
          end if;
          exit when eofile;
          --  If there was no sliding:
          --     strstart <= WSIZE+MAX_DIST-1 && lookahead <= MIN_LOOKAHEAD - 1 &&
          --     more == window_size - lookahead - strstart
          --  => more >= window_size - (MIN_LOOKAHEAD-1 + WSIZE + MAX_DIST-1)
          --  => more >= window_size - 2*WSIZE + 2
          --  In the MMAP or BIG_MEM case (not yet supported in gzip),
          --    window_size == input_size + MIN_LOOKAHEAD  &&
          --    strstart + lookahead <= input_size => more >= MIN_LOOKAHEAD.
          --  Otherwise, window_size == 2*WSIZE so more >= 2.
          --  If there was sliding, more >= WSIZE. So in all cases, more >= 2.
          --
          --  Assert(more >= 2, "more < 2");
          --
          Read_buf (strstart + lookahead, more, n);
          if n = 0 then
            eofile := True;
          else
            lookahead := lookahead + n;
          end if;
          exit when lookahead >= MIN_LOOKAHEAD or eofile;
        end loop;
        --  put_line("Fill done - eofile = " & eofile'img);
      end Fill_window;

      --  Initialize the "longest match" routines for a new file
      --
      --  IN assertion: window_size is > 0 if the input file is already read or
      --     mapped in the window array, 0 otherwise. In the first case,
      --     window_size is sufficient to contain the whole input file plus
      --     MIN_LOOKAHEAD bytes (to avoid referencing memory beyond the end
      --     of window when looking for matches towards the end).

      procedure LM_Init (pack_level : Natural) is
      begin
        --  Do not slide the window if the whole input is already in memory (window_size > 0)
        sliding := False;
        if window_size = 0 then
          sliding := True;
          window_size := 2 * ulg (WSIZE);
        end if;
        --  Initialize the hash table.
        --  prev will be initialized on the fly.
        head := (others => NIL);
        --  Set the default configuration parameters:
        max_lazy_match   := configuration_table (pack_level).max_lazy;
        good_match       := configuration_table (pack_level).good_length;
        nice_match       := configuration_table (pack_level).nice_length;
        max_chain_length := configuration_table (pack_level).max_chain;
        --  Info-Zip comment: ??? reduce max_chain_length for binary files
        strstart := 0;
        Read_buf (0, unsigned (WSIZE), lookahead);
        if lookahead = 0 then
          eofile := True;
          return;
        end if;
        eofile := False;
        --  Make sure that we always have enough lookahead. This is important
        --  if input comes from a device such as a tty.
        if lookahead < MIN_LOOKAHEAD then
          Fill_window;
        end if;
        ins_h := 0;
        for j in 0 .. Natural_M32 (MIN_MATCH) - 2 loop
          UPDATE_HASH (ins_h, window (j));
        end loop;
        --  If lookahead < MIN_MATCH, ins_h is garbage, but this is
        --  not important since only literal bytes will be emitted.
      end LM_Init;

      --  Set match_start to the longest match starting at the given string and
      --  return its length. Matches shorter or equal to prev_length are discarded,
      --  in which case the result is equal to prev_length and match_start is
      --  garbage.
      --  IN assertions: current_match is the head of the hash chain for the current
      --    string (strstart) and its distance is <= MAX_DIST, and prev_length >= 1

      procedure Longest_Match (current_match : in out Integer_M32; longest : out Integer_M32) is
        chain_length : unsigned := max_chain_length;  --  max hash chain length
        scan         : Integer_M32 := strstart;       --  current string
        match        : Integer_M32;                   --  matched string
        len          : Integer_M32;                   --  length of current match
        best_len     : Integer_M32 := prev_length;    --  best match length so far
        limit        : Natural_M32;  --  [was: IPos]
        strend       : constant Integer_M32 := strstart + MAX_MATCH;
        scan_end     : Integer_M32 := scan + best_len;
      begin
        --  Stop when current_match becomes <= limit. To simplify the code,
        --  we prevent matches with the string of window index 0.
        if strstart > MAX_DIST then
          limit := strstart - MAX_DIST;
        else
          limit := NIL;
        end if;
        --  Do not waste too much time if we already have a good match:
        if prev_length >= good_match then
          chain_length := chain_length / 4;
        end if;
        --  Assert(strstart <= window_size-MIN_LOOKAHEAD, "insufficient lookahead");
        loop
          --  Assert(current_match < strstart, "no future");
          match := current_match;
          --  Skip to next match if the match length cannot increase
          --  or if the match length is less than 2:
          --
          --  NB: this is the Not-UNALIGNED_OK variant in the C code.
          --      Translation of the UNALIGNED_OK variant is left as an exercise ;-).
          --      (!! worth a try: GNAT optimizes window(match..match+1[3]) to 16[32] bit)
          --
          if window (match + best_len)     /= window (scan_end) or else
             window (match + best_len - 1) /= window (scan_end - 1) or else
             window (match)                /= window (scan) or else
             window (match + 1)            /= window (scan + 1)
          then
            match := match + 1;  --  C: continue
          else
            --  The check at best_len - 1 can be removed because it will be made
            --  again later. (This heuristic is not always a win.)
            --
            --  It is not necessary to compare window(scan + 2) and window(match + 2) since they
            --  are always equal when the other bytes match, given that
            --  the hash keys are equal and that HASH_BITS >= 8.
            scan := scan + 2;
            match := match + 2;
            --  C: The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
            --     It is easy to get rid of this optimization if necessary.
            --  Ada: see the "else" part below.
            if MAX_MATCH = 258 then
              --  We check for insufficient lookahead only every 8th comparison;
              --  the 256th check will be made at strstart + 258.
              loop
                scan := scan + 1;
                match := match + 1;
                exit when window (scan) /= window (match);
                scan := scan + 1;
                match := match + 1;
                exit when window (scan) /= window (match);
                scan := scan + 1;
                match := match + 1;
                exit when window (scan) /= window (match);
                scan := scan + 1;
                match := match + 1;
                exit when window (scan) /= window (match);
                scan := scan + 1;
                match := match + 1;
                exit when window (scan) /= window (match);
                scan := scan + 1;
                match := match + 1;
                exit when window (scan) /= window (match);
                scan := scan + 1;
                match := match + 1;
                exit when window (scan) /= window (match);
                scan := scan + 1;
                match := match + 1;
                exit when window (scan) /= window (match) or else scan >= strend;
              end loop;
            else
              --  We check for insufficient lookahead after every comparison.
              loop
                scan := scan + 1;
                match := match + 1;
                exit when window (scan) /= window (match) or else scan >= strend;
              end loop;
            end if;
            --  Assert(scan <= window+(unsigned)(window_size-1), "wild scan");
            len := MAX_MATCH - (strend - scan);
            scan := strend - MAX_MATCH;
            if len > best_len then
              match_start := current_match;
              best_len := len;
              exit when len >= nice_match;
              scan_end  := scan + best_len;
            end if;
          end if;
          current_match := Integer_M32 (prev (unsigned (current_match) and WMASK));
          exit when current_match <= limit;
          chain_length := chain_length - 1;
          exit when chain_length = 0;
        end loop;
        longest := best_len;
      end Longest_Match;

      procedure LZ77_part_of_IZ_Deflate is
        hash_head  : Natural_M32 := NIL;              --  head of hash chain
        prev_match : Natural_M32;                     --  previous match  [was: IPos]
        match_available : Boolean := False;           --  set if previous match exists
        match_length : Natural_M32 := MIN_MATCH - 1;  --  length of best match
        max_insert : Natural_M32;
      begin
        match_start := 0;  --  NB: no initialization in deflate.c
        --  NB: level <= 3 would call deflate_fast;
        --
        --  Process the input block.
        while lookahead /= 0 loop
          --  Insert the string window(strstart .. strstart + 2) in the
          --  dictionary, and set hash_head to the head of the hash chain:
          if lookahead >= MIN_MATCH then
            INSERT_STRING (strstart, hash_head);
          end if;
          --  Find the longest match, discarding those <= prev_length.
          prev_length  := match_length;
          prev_match   := match_start;
          match_length := MIN_MATCH - 1;
          if hash_head /= NIL and then
             prev_length < max_lazy_match and then
             strstart - hash_head <= MAX_DIST
          then
            --  To simplify the code, we prevent matches with the string
            --  of window index 0 (in particular we have to avoid a match
            --  of the string with itself at the start of the input file).
            --
            --  Do not look for matches beyond the end of the input.
            --  This is necessary to make deflate deterministic.
            if nice_match > lookahead then
              nice_match := lookahead;
            end if;
            Longest_Match (hash_head, match_length);
            --  Longest_Match sets match_start
            if match_length > lookahead then
              match_length := lookahead;
            end if;
            --  Ignore a length 3 match if it is too distant:
            if match_length = MIN_MATCH and then strstart - match_start > TOO_FAR then
              --  If prev_match is also MIN_MATCH, match_start is garbage
              --  but we will ignore the current match anyway.
              match_length := MIN_MATCH - 1;
            end if;
          end if;
          --  If there was a match at the previous step and the current
          --  match is not better, output the previous match:
          if prev_length >= MIN_MATCH and then match_length <= prev_length then
            max_insert := strstart + lookahead - MIN_MATCH;
            --  C: in DEBUG mode: check_match(strstart-1, prev_match, prev_length);
            --
            ------------------------------------
            --  Output a Distance-Length code --
            ------------------------------------
            Write_DL_code (Positive (strstart - 1 - prev_match), Positive (prev_length));
            --  Insert in hash table all strings up to the end of the match.
            --  strstart-1 and strstart are already inserted.
            lookahead := lookahead - (prev_length - 1);
            prev_length := prev_length - 2;
            loop
              strstart := strstart + 1;
              if strstart <= max_insert then
                INSERT_STRING (strstart, hash_head);
                --  strstart never exceeds WSIZE - MAX_MATCH, so there
                --  are always MIN_MATCH bytes ahead.
              end if;
              prev_length := prev_length - 1;
              exit when prev_length = 0;
            end loop;
            strstart := strstart + 1;
            match_available := False;
            match_length := MIN_MATCH - 1;
          elsif match_available then
            --  If there was no match at the previous position, output a
            --  single literal. If there was a match but the current match
            --  is longer, truncate the previous match to a single literal.
            --
            ------------------------
            --  Output a literal  --
            ------------------------
            Write_literal (window (strstart - 1));
            strstart := strstart + 1;
            lookahead := lookahead - 1;
          else
            --  There is no previous match to compare with, wait for the next step to decide.
            match_available := True;
            strstart := strstart + 1;
            lookahead := lookahead - 1;
          end if;
          --  Assert(strstart <= isize && lookahead <= isize, "a bit too far");
          --
          --  Make sure that we always have enough lookahead, except
          --  at the end of the input file. We need MAX_MATCH bytes
          --  for the next match, plus MIN_MATCH bytes to insert the
          --  string following the next match.
          if lookahead < MIN_LOOKAHEAD then
            Fill_window;
          end if;
        end loop;
        -----------------------------------
        --  Output last literal, if any  --
        -----------------------------------
        if match_available then
          Write_literal (window (strstart - 1));
        end if;
      end LZ77_part_of_IZ_Deflate;

      Code_too_clever : exception;
    begin
      if Look_Ahead /= 258 or String_buffer_size /= 2 ** 15 or Threshold /= 2 then
        raise Code_too_clever;  --  was optimized for these parameters
      end if;
      window_size := 0;
      LM_Init (level);
      LZ77_part_of_IZ_Deflate;
    end LZ77_using_IZ;

    ---------------------------------------------------------------------
    --  BT4  -  Binary tree of match positions selected with           --
    --          the leading 2 to 4 bytes of each possible match.       --
    ---------------------------------------------------------------------

    --  Based on BT4.java by Lasse Collin, itself based on LzFind.c by Igor Pavlov.

    procedure LZ77_using_BT4 is
      MATCH_LEN_MIN : constant Integer := Threshold + 1;
      --
      readPos     : Integer := -1;
      cur_literal : Byte;
      readLimit   : Integer := -1;
      finishing   : constant Boolean := False;
      writePos    : Integer :=  0;
      pendingSize : Integer :=  0;
      --
      OPTS : constant := 4096;
      EXTRA_SIZE_BEFORE : constant :=  OPTS;
      EXTRA_SIZE_AFTER  : constant :=  OPTS;

      keepSizeBefore : constant Integer := EXTRA_SIZE_BEFORE + String_buffer_size;
      keepSizeAfter  : constant Integer := EXTRA_SIZE_AFTER  + Look_Ahead;
      reserveSize : constant Integer :=
        Integer'Min (
          String_buffer_size / 2 +
          256 * (2 ** 10),  --  256 KiB
          512 * (2 ** 20)   --  512 MiB
        );
      getBufSize : constant Integer := keepSizeBefore + keepSizeAfter + reserveSize;

      type Int_array is array (Natural range <>) of Integer;
      type p_Int_array is access Int_array;
      procedure Dispose is new Ada.Unchecked_Deallocation (Int_array, p_Int_array);

      procedure Normalize (positions : in out Int_array; normalizationOffset : Integer) is
      begin
        for i in 0 .. positions'Length - 1 loop
          if positions (i) <= normalizationOffset then
            positions (i) := 0;
          else
            positions (i) := positions (i) - normalizationOffset;
          end if;
        end loop;
      end Normalize;

      function getAvail return Integer is
      pragma Inline (getAvail);
      begin
         --  !! - 1 added for getting readPos in buf'Range upon: cur_literal:= buf(readPos);
        return writePos - readPos - 1;
      end getAvail;

      function movePos (requiredForFlushing, requiredForFinishing : Integer) return Integer is
        avail : Integer;
      begin
        --  assert requiredForFlushing >= requiredForFinishing;
        readPos := readPos + 1;
        avail   := getAvail;
        if avail < requiredForFlushing then
          if avail < requiredForFinishing or else not finishing
          then
            pendingSize := pendingSize + 1;
            --  GdM: this causes cyclicPos and lzpos not being in sync with readPos,
            --       the pendingSize value is there for catching up.
            avail := 0;
          end if;
        end if;
        return avail;
      end movePos;

      function getHash4Size return Integer is
        h : Unsigned_32 := Unsigned_32 (String_buffer_size - 1);
      begin
        h := h or Shift_Right (h, 1);
        h := h or Shift_Right (h, 2);
        h := h or Shift_Right (h, 4);
        h := h or Shift_Right (h, 8);
        h := Shift_Right (h, 1);
        h := h or 16#FFFF#;  --  LzFind.c: "don't change it! It's required for Deflate"
        if h > 2 ** 24 then
          h := Shift_Right (h, 1);
        end if;
        return Integer (h + 1);
      end getHash4Size;

      type Byte_array is array (Natural range <>) of Byte;
      type p_Byte_array is access Byte_array;
      procedure Dispose is new Ada.Unchecked_Deallocation (Byte_array, p_Byte_array);

      package Hash234 is
        HASH_2_SIZE : constant := 2 ** 10;
        HASH_2_MASK : constant := HASH_2_SIZE - 1;
        HASH_3_SIZE : constant := 2 ** 16;
        HASH_3_MASK : constant := HASH_3_SIZE - 1;
        hash_4_size : constant Integer := getHash4Size;
        hash_4_mask : constant Unsigned_32 := Unsigned_32 (hash_4_size) - 1;
        --
        hash2Table : Int_array (0 .. HASH_2_SIZE - 1) := (others => 0);  --  [Initialization added]
        hash3Table : Int_array (0 .. HASH_3_SIZE - 1) := (others => 0);  --  [Initialization added]
        hash4Table : p_Int_array;
        --
        hash2Value, hash3Value, hash4Value : Unsigned_32 := 0;
        --
        procedure calcHashes (buf : Byte_array; off : Integer);
        procedure updateTables (pos : Integer);
        procedure Normalize (normalizeOffset : Integer);
      end Hash234;

      package body Hash234 is

        crcTable : array (Byte) of Unsigned_32;
        CRC32_POLY : constant := 16#EDB8_8320#;

        procedure calcHashes (buf : Byte_array; off : Integer) is
          temp : Unsigned_32 := crcTable (buf (off)) xor Unsigned_32 (buf (off + 1));
        begin
          hash2Value := temp and HASH_2_MASK;
          temp := temp xor Shift_Left (Unsigned_32 (buf (off + 2)), 8);
          hash3Value := temp and HASH_3_MASK;
          temp := temp xor Shift_Left (crcTable (buf (off + 3)), 5);
          hash4Value := temp and hash_4_mask;
        end calcHashes;

        procedure updateTables (pos : Integer) is
        begin
          hash2Table (Integer (hash2Value)) := pos;
          hash3Table (Integer (hash3Value)) := pos;
          hash4Table (Integer (hash4Value)) := pos;
        end updateTables;

        procedure Normalize (normalizeOffset : Integer) is
        begin
          Normalize (hash2Table, normalizeOffset);
          Normalize (hash3Table, normalizeOffset);
          Normalize (hash4Table.all, normalizeOffset);
        end Normalize;

        r : Unsigned_32;
      begin
        --  NB: heap allocation used only for convenience because of
        --      small default stack sizes on some compilers.
        hash4Table := new Int_array (0 .. hash_4_size - 1);
        hash4Table.all := (others => 0);  --  [Initialization added]
        for i in Byte loop
          r := Unsigned_32 (i);
          for j in 0 .. 7 loop
            if (r and 1) = 0 then
              r := Shift_Right (r, 1);
            else
              r := Shift_Right (r, 1) xor CRC32_POLY;
            end if;
          end loop;
          crcTable (i) := r;
        end loop;
      end Hash234;

      niceLen : constant Integer := Integer'Min (162, Look_Ahead);  --  const. was 64
      depthLimit : constant := 48;  --  Alternatively: 16 + niceLen / 2

      --  !! nicer: unconstrained array of (dist, len) pairs, 1-based array.

      type Any_Matches_type (countMax : Integer) is record
        count : Integer := 0;
        len   : Int_array (0 .. countMax);
        dist  : Int_array (0 .. countMax);
      end record;

      --  Subtracting 1 because the shortest match that this match
      --  finder can find is 2 bytes, so there's no need to reserve
      --  space for one-byte matches.
      subtype Matches_type is Any_Matches_type (niceLen - 1);

      cyclicSize : constant Integer := String_buffer_size;  --  Had: + 1;
      cyclicPos  : Integer := -1;
      lzPos      : Integer := cyclicSize;

      max_dist : constant Integer := cyclicSize;

      package BT4_Algo is
        procedure skip (len : Natural);
        pragma Inline (skip);
        function getMatches return Matches_type;
      end BT4_Algo;

      buf : p_Byte_array;
      tree : p_Int_array;

      package body BT4_Algo is

        function movePos return Integer is
          avail : constant Integer := movePos (requiredForFlushing => niceLen, requiredForFinishing => 4);
          normalizationOffset : Integer;
        begin
          --  Put_Line("BT4_movePos");
          if avail /= 0 then
            lzPos := lzPos + 1;
            if lzPos = Integer'Last then
              normalizationOffset := Integer'Last - cyclicSize;
              Hash234.Normalize (normalizationOffset);
              Normalize (tree.all, normalizationOffset);
              lzPos := lzPos - normalizationOffset;
            end if;
            cyclicPos := cyclicPos + 1;
            if cyclicPos = cyclicSize then
              --  Put_Line("cyclicPos zeroed");
              cyclicPos := 0;
            end if;
          end if;
          return avail;
        end movePos;

        Null_position : constant := -1;  --  LzFind.c: kEmptyHashValue, 0

        procedure skip_update_tree (niceLenLimit : Integer; currentMatch : in out Integer) is
          delta0, depth, ptr0, ptr1, pair, len, len0, len1 : Integer;
        begin
          --  Put("BT4.skip_update_tree... ");
          depth := depthLimit;
          ptr0  := cyclicPos * 2 + 1;
          ptr1  := cyclicPos * 2;
          len0  := 0;
          len1  := 0;
          loop
            delta0 := lzPos - currentMatch;
            if depth = 0 or else delta0 >= max_dist then
              tree (ptr0) := Null_position;
              tree (ptr1) := Null_position;
              return;
            end if;
            depth := depth - 1;
            if cyclicPos - delta0 < 0 then
              pair := cyclicSize;
            else
              pair := 0;
            end if;
            pair := (cyclicPos - delta0 + pair) * 2;
            len  := Integer'Min (len0, len1);
            --  Match ?
            if buf (readPos + len - delta0) = buf (readPos + len) then
              --  No need to look for longer matches than niceLenLimit
              --  because we only are updating the tree, not returning
              --  matches found to the caller.
              loop
                len := len + 1;
                if len = niceLenLimit then
                  tree (ptr1) := tree (pair);
                  tree (ptr0) := tree (pair + 1);
                  return;
                end if;
                exit when buf (readPos + len - delta0) /= buf (readPos + len);
              end loop;
            end if;
            --  Bytes are no more matching. The past value is either smaller...
            if buf (readPos + len - delta0) < buf (readPos + len) then
              tree (ptr1) := currentMatch;
              ptr1 := pair + 1;
              currentMatch := tree (ptr1);
              len1 := len;
            else  --  ... or larger
              tree (ptr0) := currentMatch;
              ptr0 := pair;
              currentMatch := tree (ptr0);
              len0 := len;
            end if;
          end loop;
        end skip_update_tree;

        procedure skip (len : Natural) is
          --
          procedure Skip_one is
          pragma Inline (Skip_one);
            niceLenLimit, avail, currentMatch : Integer;
          begin
            niceLenLimit := niceLen;
            avail := movePos;
            if avail < niceLenLimit then
              if avail = 0 then
                return;
              end if;
              niceLenLimit := avail;
            end if;
            Hash234.calcHashes (buf.all, readPos);
            currentMatch := Hash234.hash4Table (Integer (Hash234.hash4Value));
            Hash234.updateTables (lzPos);
            skip_update_tree (niceLenLimit, currentMatch);
          end Skip_one;
          --
        begin
          for count in reverse 1 .. len loop
            Skip_one;
          end loop;
        end skip;

        function getMatches return Matches_type is
          matches : Matches_type;
          matchLenLimit : Integer := Look_Ahead;
          niceLenLimit  : Integer := niceLen;
          avail : Integer;
          delta0, delta2, delta3, currentMatch,
          lenBest, depth, ptr0, ptr1, pair, len, len0, len1 : Integer;
        begin
          --  Put("BT4.getMatches... ");
          matches.count := 0;
          avail := movePos;
          if avail < matchLenLimit then
            if avail = 0 then
              return matches;
            end if;
            matchLenLimit := avail;
            if niceLenLimit > avail then
              niceLenLimit := avail;
            end if;
          end if;
          --
          Hash234.calcHashes (buf.all, readPos);
          delta2 := lzPos - Hash234.hash2Table (Integer (Hash234.hash2Value));
          delta3 := lzPos - Hash234.hash3Table (Integer (Hash234.hash3Value));
          currentMatch :=   Hash234.hash4Table (Integer (Hash234.hash4Value));
          Hash234.updateTables (lzPos);
          --
          lenBest := 0;
          --  See if the hash from the first two bytes found a match.
          --  The hashing algorithm guarantees that if the first byte
          --  matches, also the second byte does, so there's no need to
          --  test the second byte.
          if delta2 < max_dist and then buf (readPos - delta2) = buf (readPos) then
            --  Match of length 2 found and checked.
            lenBest := 2;
            matches.len (0) := 2;
            matches.dist (0) := delta2 - 1;
            matches.count := 1;
          end if;
          --  See if the hash from the first three bytes found a match that
          --  is different from the match possibly found by the two-byte hash.
          --  Also here the hashing algorithm guarantees that if the first byte
          --  matches, also the next two bytes do.
          if delta2 /= delta3 and then delta3 < max_dist
                  and then buf (readPos - delta3) = buf (readPos)
          then
            --  Match of length 3 found and checked.
            lenBest := 3;
            matches.count := matches.count + 1;
            matches.dist (matches.count - 1) := delta3 - 1;
            delta2 := delta3;
          end if;
          --  If a match was found, see how long it is.
          if matches.count > 0 then
            while lenBest < matchLenLimit and then buf (readPos + lenBest - delta2)
                                                 = buf (readPos + lenBest)
            loop
              lenBest := lenBest + 1;
            end loop;
            matches.len (matches.count - 1) := lenBest;
            --  Return if it is long enough (niceLen or reached the end of the dictionary).
            if lenBest >= niceLenLimit then
              skip_update_tree (niceLenLimit, currentMatch);
              return matches;
            end if;
          end if;
          --  Long enough match wasn't found so easily. Look for better matches from the binary tree.
          if lenBest < 3 then
            lenBest := 3;
          end if;
          depth := depthLimit;
          ptr0  := cyclicPos * 2 + 1;
          ptr1  := cyclicPos * 2;
          len0  := 0;
          len1  := 0;
          --
          loop
            delta0 := lzPos - currentMatch;
            --  Return if the search depth limit has been reached or
            --  if the distance of the potential match exceeds the
            --  dictionary size.
            if depth = 0 or else delta0 >= max_dist then
              tree (ptr0) := Null_position;
              tree (ptr1) := Null_position;
              return matches;
            end if;
            depth := depth - 1;
            --
            if cyclicPos - delta0 < 0 then
              pair := cyclicSize;
            else
              pair := 0;
            end if;
            pair := (cyclicPos - delta0 + pair) * 2;
            len  := Integer'Min (len0, len1);
            --  Match ?
            if buf (readPos + len - delta0) = buf (readPos + len) then
              loop
                len := len + 1;
                exit when len >= matchLenLimit
                  or else buf (readPos + len - delta0) /= buf (readPos + len);
              end loop;
              if len > lenBest then
                lenBest := len;
                matches.len (matches.count) := len;
                matches.dist (matches.count) := delta0 - 1;
                matches.count := matches.count + 1;
                if len >= niceLenLimit then
                  tree (ptr1) := tree (pair);
                  tree (ptr0) := tree (pair + 1);
                  return matches;
                end if;
              end if;
            end if;
            --  Bytes are no more matching. The past value is either smaller...
            if buf (readPos + len - delta0) < buf (readPos + len) then
              tree (ptr1) := currentMatch;
              ptr1 := pair + 1;
              currentMatch := tree (ptr1);
              len1 := len;
            else  --  ... or larger
              tree (ptr0) := currentMatch;
              ptr0 := pair;
              currentMatch := tree (ptr0);
              len0 := len;
            end if;
          end loop;
        end getMatches;

      begin
        --  NB: heap allocation used only for convenience because of
        --      small default stack sizes on some compilers.
        tree := new Int_array (0 .. cyclicSize * 2 - 1);
        tree.all := (others => Null_position);
      end BT4_Algo;

      --  Moves data from the end of the buffer to the beginning, discarding
      --  old data and making space for new input.

      procedure moveWindow is
        --  Align the move to a multiple of 16 bytes (LZMA-friendly, see pos_bits)
        moveOffset : constant Integer := ((readPos + 1 - keepSizeBefore) / 16) * 16;
        moveSize   : constant Integer := writePos - moveOffset;
      begin
        --  Put_Line("  Move window, size=" & moveSize'Img & " offset=" & moveOffset'Img);
        buf (0 .. moveSize - 1) := buf (moveOffset .. moveOffset + moveSize - 1);
        readPos   := readPos   - moveOffset;
        readLimit := readLimit - moveOffset;
        writePos  := writePos  - moveOffset;
      end moveWindow;

     --  Copies new data into the buffer.
     function fillWindow (len_initial : Integer) return Integer is

       --  Process pending data that hasn't been ran through the match finder yet.
       --  Run it through the match finder now if there is enough new data
       --  available (readPos < readLimit) that the encoder may encode at
       --  least one more input byte.
       --
       procedure processPendingBytes is
         oldPendingSize : Integer;
       begin
         if pendingSize > 0 and then readPos < readLimit then
           readPos := readPos - pendingSize;
           oldPendingSize := pendingSize;
           pendingSize := 0;
           BT4_Algo.skip (oldPendingSize);
         end if;
       end processPendingBytes;
       --
       len : Integer := len_initial;
       actual_len : Integer := 0;
     begin
        --  Put_Line("Fill window - start");
        --  Move the sliding window if needed.
        if readPos >= buf'Length - keepSizeAfter then
          moveWindow;
        end if;

        --  Try to fill the dictionary buffer up to its boundary.
        if len > buf'Length - writePos then
          len := buf'Length - writePos;
        end if;

        while len > 0 and then More_bytes loop
          buf (writePos) := Read_byte;
          writePos := writePos + 1;
          len := len - 1;
          actual_len := actual_len + 1;
        end loop;

        --  Set the new readLimit but only if there's enough data to allow
        --  encoding of at least one more byte.
        if writePos >= keepSizeAfter then
          readLimit := writePos - keepSizeAfter;
        end if;

        processPendingBytes;

        --  Put_Line("Fill window, requested=" & len_initial'Img & " actual=" & actual_len'Img);
        --  Tell the caller how much input we actually copied into the dictionary.
        return actual_len;
      end fillWindow;

      matches : Matches_type;
      readAhead : Integer := -1;  -- LZMAEncoder.java

      function getMatches return Matches_type is
      begin
        readAhead := readAhead + 1;
        return BT4_Algo.getMatches;
      end getMatches;

      procedure skip (len : Natural) is
      pragma Inline (skip);
      begin
        readAhead := readAhead + len;
        BT4_Algo.skip (len);
      end skip;

      --  Small stack of recent distances used for LZ.
      subtype Repeat_stack_range is Integer range 0 .. 3;
      rep_dist : array (Repeat_stack_range) of Natural := (others => 0);

      procedure getNextSymbol is
        avail, mainLen, mainDist, newLen, newDist, limit : Integer;

        function changePair (smallDist, bigDist : Integer) return Boolean is
        pragma Inline (changePair);
        begin
          return smallDist < bigDist / 128;
        end changePair;

        --  This function is for debugging. The matches stored in the 'tree' array
        --  may be wrong if the variables cyclicPos, lzPos and readPos are not in sync.
        --  The issue seems to have been solved now (rev. 489).
        function Is_match_correct (shift : Natural) return Boolean is
        pragma Inline (Is_match_correct);
          paranoid : constant Boolean := True;
        begin
          if paranoid then
            for i in reverse -1 + shift .. mainLen - 2 + shift loop
              if buf (readPos - (mainDist + 1) + i) /= buf (readPos + i) then
                return False;  --  Should not occur (check with code coverage)
              end if;
            end loop;
          end if;
          return True;
        end Is_match_correct;

        function getMatchLen (dist, lenLimit : Integer) return Natural is
        pragma Inline (getMatchLen);
          backPos : constant Integer := readPos - dist - 1;
          len : Integer := 0;
        begin
          if dist < 1 then
            return 0;
          end if;
          --  @ if readPos+len not in buf.all'Range then
          --  @   Put("**** readpos " & buf'Last'Img & readPos'Img);
          --  @ end if;
          --  @ if backPos+len not in buf.all'Range then
          --  @   Put("**** backpos " & buf'Last'Img & backPos'Img);
          --  @ end if;
          while len < lenLimit and then buf (readPos + len) = buf (backPos + len) loop
            len := len + 1;
          end loop;
          return len;
        end getMatchLen;

        procedure Send_first_literal_of_match is
        begin
          Write_literal (cur_literal);
          readAhead := readAhead - 1;
        end Send_first_literal_of_match;

        procedure Send_DL_code (distance, length : Integer) is
          found_repeat : Integer := rep_dist'First - 1;
          aux : Integer;
        begin
          Write_DL_code (distance + 1, length);
          readAhead := readAhead - length;
          if LZMA_friendly then
            --
            --  Manage the stack of recent distances in the same way the "MA" part of LZMA does.
            --
            for i in rep_dist'Range loop
              if distance = rep_dist (i) then
                found_repeat := i;
                exit;
              end if;
            end loop;
            if found_repeat >= rep_dist'First then
              --  Roll the stack of recent distances up to the item with index found_repeat,
              --  which becomes first. If found_repeat = rep_dist'First, no actual change occurs.
              aux := rep_dist (found_repeat);
              for i in reverse rep_dist'First + 1 .. found_repeat loop
                rep_dist (i) := rep_dist (i - 1);
              end loop;
              rep_dist (rep_dist'First) := aux;
            else
              --  Shift the stack of recent distances; the new distance becomes the first item.
              for i in reverse rep_dist'First + 1 .. rep_dist'Last loop
                rep_dist (i) := rep_dist (i - 1);
              end loop;
              rep_dist (0) := distance;
            end if;
          end if;
        end Send_DL_code;

        bestRepLen, bestRepIndex, len : Integer;

      begin
        --  Get the matches for the next byte unless readAhead indicates
        --  that we already got the new matches during the previous call
        --  to this procedure.
        if readAhead = -1 then
          matches := getMatches;
        end if;
        --  @ if readPos not in buf.all'Range then
        --  @   Put("**** " & buf'Last'Img & keepSizeAfter'Img & readPos'Img & writePos'Img);
        --  @ end if;
        cur_literal := buf (readPos);
        --  Get the number of bytes available in the dictionary, but
        --  not more than the maximum match length. If there aren't
        --  enough bytes remaining to encode a match at all, return
        --  immediately to encode this byte as a literal.
        avail := Integer'Min (getAvail, Look_Ahead);
        if avail < MATCH_LEN_MIN then
          --  Put("[a]");
          Send_first_literal_of_match;
          return;
        end if;

        if LZMA_friendly then
          --  Look for a match from the previous four different match distances.
          bestRepLen := 0;
          bestRepIndex := 0;
          for rep in Repeat_stack_range loop
            len := getMatchLen (rep_dist (rep), avail);
            if len >= MATCH_LEN_MIN then
              --  If it is long enough, return it.
              if len >= niceLen then
                skip (len - 1);
                --  Put_Line("[DL RA]");
                Send_DL_code (rep_dist (rep), len);
                return;
              end if;
              --  Remember the index and length of the best repeated match.
              if len > bestRepLen then
                bestRepIndex := rep;
                bestRepLen := len;
              end if;
            end if;
          end loop;
        end if;

        mainLen := 0;
        mainDist := 0;
        if matches.count > 0 then
          mainLen  := matches.len (matches.count - 1);
          mainDist := matches.dist (matches.count - 1);
          if mainLen >= niceLen then
            if Is_match_correct (1) then
              skip (mainLen - 1);
              --  Put_Line("[DL A]" & mainDist'Img & mainLen'Img);
              Send_DL_code (mainDist, mainLen);
              return;
            else
              --  Put_Line("Wrong match [A]! pos=" & Integer'Image(lzPos - cyclicSize));
              Send_first_literal_of_match;
              return;
            end if;
          end if;
          while matches.count > 1 and then mainLen = matches.len (matches.count - 2) + 1 loop
            exit when not changePair (matches.dist (matches.count - 2), mainDist);
            matches.count := matches.count - 1;
            mainLen  := matches.len (matches.count - 1);
            mainDist := matches.dist (matches.count - 1);
          end loop;
          if mainLen = MATCH_LEN_MIN and then mainDist >= 128 then
            mainLen := 1;
          end if;
        end if;

        if LZMA_friendly
             and then bestRepLen >= MATCH_LEN_MIN
             and then (bestRepLen + 1 >= mainLen
                        or else (bestRepLen + 2 >= mainLen and then mainDist >= 2 ** 9)
                        or else (bestRepLen + 3 >= mainLen and then mainDist >= 2 ** 15))
        then
          skip (bestRepLen - 1);
          --  Put_Line("[DL RB]");
          Send_DL_code (rep_dist (bestRepIndex), bestRepLen);
          return;
        end if;

        if mainLen < MATCH_LEN_MIN or else avail <= MATCH_LEN_MIN then
          --  Put("[b]");
          Send_first_literal_of_match;
          return;
        end if;

        --  Get the next match. Test if it is better than the current match.
        --  If so, encode the current byte as a literal.
        matches := getMatches;
        --
        if matches.count > 0 then
          newLen  := matches.len (matches.count - 1);
          newDist := matches.dist (matches.count - 1);
          if (newLen >= mainLen and then newDist < mainDist)
                  or else (newLen = mainLen + 1
                      and then not changePair (mainDist, newDist))
                  or else newLen > mainLen + 1
                  or else (newLen + 1 >= mainLen
                      and then mainLen >= MATCH_LEN_MIN + 1
                      and then changePair (newDist, mainDist))
          then
            --  Put("[c]");
            --  Put(Character'Val(cur_literal));
            Send_first_literal_of_match;
            return;
          end if;
        end if;

        limit := Integer'Max (mainLen - 1, MATCH_LEN_MIN);
        for rep in rep_dist'Range loop
          if getMatchLen (rep_dist (rep), limit) = limit then
            Send_first_literal_of_match;
            return;
          end if;
        end loop;

        if Is_match_correct (0) then
          skip (mainLen - 2);
          --  Put_Line("[DL B]" & mainDist'Img & mainLen'Img);
          Send_DL_code (mainDist, mainLen);
        else
          --  Put_Line("Wrong match [B]!");
          Send_first_literal_of_match;
        end if;
      end getNextSymbol;

      actual_written, avail : Integer;
    begin
      --  NB: heap allocation used only for convenience because of
      --      small default stack sizes on some compilers.
      buf := new Byte_array (0 .. getBufSize);
      actual_written := fillWindow (String_buffer_size);
      if actual_written > 0 then
        loop
          getNextSymbol;
          avail := getAvail;
          if avail = 0 then
            actual_written := fillWindow (String_buffer_size);
            exit when actual_written = 0;
          end if;
        end loop;
      end if;
      Dispose (buf);
      Dispose (tree);
      Dispose (Hash234.hash4Table);
    end LZ77_using_BT4;

    procedure LZ77_by_Rich is
      --  * PROG2.C [lz77a.c]                                             *
      --  * Simple Hashing LZ77 Sliding Dictionary Compression Program    *
      --  * By Rich Geldreich, Jr. October, 1993                          *
      --  * Originally compiled with QuickC v2.5 in the small model.      *
      --  * This program uses more efficient code to delete strings from  *
      --  * the sliding dictionary compared to PROG1.C, at the expense of *
      --  * greater memory requirements. See the HashData and DeleteData  *
      --  * subroutines.                                                  *
      --
      --  Comments by GdM, 2019+ appear in square brackets: [...]

      --  Set this to True for a greedy encoder.
      GREEDY : constant Boolean := False;  --  [original: False]

      --  Ratio vs. speed constant [ Is it really a ratio? ].
      --  The larger this constant, the better the compression.
      MAXCOMPARES : constant := 4096;  --  [original: 75; good: 2400; from Info-Zip: 4096]

      --  Unused entry code.
      NIL : constant := 16#FFFF#;

      --  /* bits per symbol- normally 8 for general purpose compression */
      --  #define CHARBITS : constant := 8;  [ NB: dictionary uses char (byte) ]

      --  Minimum match length & maximum match length.
      THRESHOLD_Rich : constant := 2;
      MATCHBITS      : constant := 8;  --  [original: 4]
      --  [original: 2 ** MATCHBITS + THRESHOLD - 1]
      MAXMATCH  : constant := 2 ** MATCHBITS + THRESHOLD_Rich;  -- 258 is Deflate-friendly.

      --  Sliding dictionary size and hash table's size.
      --  Some combinations of HASHBITS and THRESHOLD values will not work
      --  correctly because of the way this program hashes strings.

      DICTBITS : constant := 15;  --  [original: 13]
      HASHBITS : constant := 13;  --  [original: 10]
      --
      DICTSIZE : constant := 2 ** DICTBITS;
      HASHSIZE : constant := 2 ** HASHBITS;

      --  # bits to shift after each XOR hash
      --  This constant must be high enough so that only THRESHOLD + 1
      --  characters are in the hash accumulator at one time.

      SHIFTBITS : constant := ((HASHBITS + THRESHOLD_Rich) / (THRESHOLD_Rich + 1));

      --  Sector size constants [the dictionary is partitoned in sectors].

      SECTORBIT : constant := 13;  --  [original: 10; OK: 13]
      SECTORLEN : constant := 2 ** SECTORBIT;

      HASH_MASK_1 : constant := 16#8000#;  --  [ was called HASHFLAG1 ]
      HASH_MASK_2 : constant := 16#7FFF#;  --  [ was called HASHFLAG2 ]

      --  Dictionary plus MAXMATCH extra chars for string comparisions.
      dict : array (Integer_M32'(0) .. DICTSIZE + MAXMATCH - 1) of Byte;

      subtype Unsigned_int is Unsigned_16;

      --  Hash table & link list tables.

      --  [ So far we index the hash table with Integer (minimum 16 bit signed) ]
      hash       : array (0 .. HASHSIZE - 1) of Unsigned_int := (others => NIL);
      --  [ nextlink: in lz77a.c: only through DICTSIZE - 1,
      --    although Init has: nextlink[DICTSIZE] = NIL. In doubt we set the
      --    'Last to DICTSIZE and fill everything with NIL... ]
      nextlink   : array (Integer_M32'(0) .. DICTSIZE)     of Unsigned_int := (others => NIL);
      lastlink   : array (Integer_M32'(0) .. DICTSIZE - 1) of Unsigned_int := (others => NIL);

      --  Loads dictionary with characters from the input stream.
      --
      procedure Load_Dict (dictpos : Integer_M32; actually_read : out Integer_M32) is
        i : Integer_M32 := 0;
      begin
        while More_bytes loop
          dict (dictpos + i) := Read_byte;
          i := i + 1;
          exit when i = SECTORLEN;
        end loop;

        --  Since the dictionary is a ring buffer, copy the characters at
        --  the very start of the dictionary to the end
        --  [this avoids having to use an "and" or a "mod" operator when searching].
        --
        if dictpos = 0 then
          for j in Integer_M32'(0) .. MAXMATCH - 1 loop
            dict (j + DICTSIZE) := dict (j);
          end loop;
        end if;

        actually_read := i;
      end Load_Dict;

      --  Deletes data from the dictionary search structures
      --  This is only done when the number of bytes to be
      --  compressed exceeds the dictionary's size.
      --
      procedure Delete_Data (dictpos : Integer_M32) is
        j, k : Integer_M32;
      begin
        --  Delete all references to the sector being deleted.
        k := dictpos + SECTORLEN;
        for i in dictpos .. k - 1 loop
          j := Integer_M32 (lastlink (i));
          if (Unsigned_int (j) and HASH_MASK_1) /= 0 then
            if j /= NIL then
              hash (Integer (Unsigned_int (j) and HASH_MASK_2)) := NIL;
            end if;
          else
            nextlink (j) := NIL;
          end if;
        end loop;
      end Delete_Data;

      --  Hash data just entered into dictionary.
      --  XOR hashing is used here, but practically any hash function will work.
      --
      procedure Hash_Data (dictpos, bytestodo : Integer_M32) is
        j : Integer;
        k : Integer_M32;
      begin
        if bytestodo <= THRESHOLD_Rich then  -- Not enough bytes in sector for match?
          nextlink (dictpos .. dictpos + bytestodo - 1) := (others => NIL);
          lastlink (dictpos .. dictpos + bytestodo - 1) := (others => NIL);
        else
          --  Matches can't cross sector boundaries.
          nextlink (dictpos + bytestodo - THRESHOLD_Rich .. dictpos + bytestodo - 1) := (others => NIL);
          lastlink (dictpos + bytestodo - THRESHOLD_Rich .. dictpos + bytestodo - 1) := (others => NIL);

          j :=  Integer (
                  Shift_Left (Unsigned_int (dict (dictpos)), SHIFTBITS)
                  xor
                  Unsigned_int (dict (dictpos + 1))
                );

          k := dictpos + bytestodo - THRESHOLD_Rich;  --  Calculate end of sector.

          for i in dictpos ..  k - 1 loop
            j := Integer (
                  (Shift_Left (Unsigned_int (j), SHIFTBITS) and (HASHSIZE - 1))
                   xor
                   Unsigned_int (dict (i + THRESHOLD_Rich))
                 );
            lastlink (i) := Unsigned_int (j) or HASH_MASK_1;
            nextlink (i) := hash (j);
            if nextlink (i) /= NIL then
              lastlink (Integer_M32 (nextlink (i))) := Unsigned_int (i);
            end if;
            hash (j) := Unsigned_int (i);
          end loop;
        end if;
      end Hash_Data;

      matchlength, matchpos : Integer_M32;

      --  Finds match for string at position dictpos.
      --  This search code finds the longest AND closest
      --  match for the string at dictpos.
      --
      procedure Find_Match (dictpos, startlen : Integer_M32) is
        i, j : Integer_M32;
        match_byte : Byte;
      begin
        i := dictpos;
        matchlength := startlen;
        match_byte := dict (dictpos + matchlength);
        --
        Chances :
        for compare_count in 1 .. MAXCOMPARES loop
          i := Integer_M32 (nextlink (i));  --  Get next string in list.
          if i = NIL then
            return;
          end if;
          --
          if dict (i + matchlength) = match_byte then  --  Possible larger match?
            j := 0;
            --  Compare strings.
            loop
              exit when dict (dictpos + j) /= dict (i + j);
              j := j + 1;
              exit when j = MAXMATCH;
            end loop;
            --
            if j > matchlength then  --  Found larger match?
              matchlength := j;
              matchpos    := i;
              if matchlength = MAXMATCH then
                return;  --  Exit if largest possible match.
              end if;
              match_byte := dict (dictpos + matchlength);
            end if;
          end if;
        end loop Chances;  --  Keep on trying until we run out of chances.
      end Find_Match;

      --  Finds dictionary matches for characters in current sector.
      --
      procedure Dict_Search (dictpos, bytestodo : Integer_M32) is
        i, j : Integer_M32;
        matchlen1, matchpos1 : Integer_M32;
        --
        procedure Write_literal_pos_i is
        pragma Inline (Write_literal_pos_i);
        begin
          Write_literal (dict (i));
          i := i + 1;
          j := j - 1;
        end Write_literal_pos_i;
      begin
        i := dictpos;
        j := bytestodo;

        if not GREEDY then  --  Non-greedy search loop (slow).

          while j /= 0 loop  --  Loop while there are still characters left to be compressed.
            Find_Match (i, THRESHOLD_Rich);

            if matchlength > THRESHOLD_Rich then
              matchlen1 := matchlength;
              matchpos1 := matchpos;

              loop
                Find_Match (i + 1, matchlen1);

                if matchlength > matchlen1 then
                  matchlen1 := matchlength;
                  matchpos1 := matchpos;
                  Write_literal_pos_i;
                else
                  if matchlen1 > j then
                    matchlen1 := j;
                    if matchlen1 <= THRESHOLD_Rich then
                      Write_literal_pos_i;
                      exit;
                    end if;
                  end if;

                  Write_DL_code (
                    length   => Integer (matchlen1),
                    --  [The subtraction happens modulo 2**n, needs to be cleaned modulo 2**DICTSIZE]
                    distance => Integer ((Unsigned_32 (i) - Unsigned_32 (matchpos1)) and (DICTSIZE - 1))
                  );
                  i := i + matchlen1;
                  j := j - matchlen1;
                  exit;
                end if;
              end loop;

            else
              Write_literal_pos_i;
            end if;

          end loop;  --  while j /= 0

        else  --  Greedy search loop (fast).

          while j /= 0 loop  --  Loop while there are still characters left to be compressed.

            Find_Match (i, THRESHOLD_Rich);

            if matchlength > j then
              matchlength := j;     --  Clamp matchlength.
            end if;

            if matchlength > THRESHOLD_Rich then  --  Valid match?
              Write_DL_code (
                length   => Integer (matchlength),
                --  [The subtraction happens modulo 2**n, needs to be cleaned modulo 2**DICTSIZE]
                distance => Integer ((Unsigned_32 (i) - Unsigned_32 (matchpos)) and (DICTSIZE - 1))
              );
              i := i + matchlength;
              j := j - matchlength;
            else
              Write_literal_pos_i;
            end if;
          end loop;

        end if;  --  Greedy or not.

      end Dict_Search;

      procedure Encode_Rich is
        dictpos, actual_read : Integer_M32 :=  0;
        deleteflag : Boolean := False;
      begin
        loop
          --  Delete old data from dictionary.
          if deleteflag then
            Delete_Data (dictpos);
          end if;

          --  Grab more data to compress.
          Load_Dict (dictpos, actual_read);
          exit when actual_read = 0;

          --  Hash the data.
          Hash_Data (dictpos, actual_read);

          --  Find dictionary matches.
          Dict_Search (dictpos, actual_read);

          dictpos := dictpos + SECTORLEN;

          --  Wrap back to beginning of dictionary when it's full.
          if dictpos = DICTSIZE then
            dictpos := 0;
            deleteflag := True;   --  Ok to delete now.
          end if;
        end loop;
      end Encode_Rich;

    begin
      Encode_Rich;
    end LZ77_by_Rich;

  begin
    case Method is
      when LZHuf =>
        LZ77_using_LZHuf;
      when IZ_4 .. IZ_10 =>
        LZ77_using_IZ (4 + Method_Type'Pos (Method) -  Method_Type'Pos (IZ_4));
      when BT4 =>
        LZ77_using_BT4;
      when Rich =>
        LZ77_by_Rich;
      when No_LZ77 =>
        while More_bytes loop
          Write_literal (Read_byte);
        end loop;
    end case;
  end Encode;

end LZ77;
