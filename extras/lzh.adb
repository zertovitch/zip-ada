--  *CAUTION* : bug on the last decoded byte (see "BUG" below)
---------------
--
--  Legal licensing note:
--
--  Copyright (c) 1999 .. 2009 Gautier de Montmollin
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php
--
--  LZHUF.C English version 1.0
--  Based on Japanese version 29-NOV-1988
--  LZSS coded by Haruhiko OKUMURA
--  Adaptive Huffman Coding coded by Haruyasu YOSHIZAKI
--  Edited and translated to English by Kenji RIKITAKE
--  Converted to Turbo Pascal 5.0
--    by Peter Sawatzki with assistance of Wayne Sullivan
--
--  Translated on 14-Jan-2000 by G. de Montmollin, using (New)P2Ada
--    then transformed into 100% portable code (OS-,compiler- independent)
--    using genericity. Buffers removed - they can be implemented outside
--    this package if needed. Data integrity checking too.

--  8-May-2002: Source reformatted and adapted according to Craig Carey's
--              (http://www.ijs.co.nz/) version and comments.
--              LZH is made thread-safe: only local variables, no shared
--              variables between Encode and Decode.

--  29-Jan-2009: No more need to know the input length; no more feedback

--  17-Oct-2018: BUG found and not yet fixed: in rare cases (e.g.
--               data = ziptest.exe compiled by GNAT for Windows x64),
--               the last decoded byte is missing. See test_non_zip for mass test.

package body LZH is

  ----- LZSS Parameters -----
  String_buffer_size : constant := 2**12; -- 2**12 = 4096
  Look_Ahead         : constant := 65;    -- Original: 60
  Threshold          : constant := 2;

  N_Char    : constant := 256 - Threshold + Look_Ahead;
  --  Character code (= 0..N_CHAR-1)
  Max_Table     : constant := N_Char * 2 - 1;

  subtype Byte is Unsigned_8;
  --  Just a nicer name. BTW, easier to modify.

  type Text_Buffer is array (0 .. String_buffer_size + Look_Ahead - 1) of Byte;
  empty_buffer : constant Text_Buffer := (others => 32); -- ' '

  --  > The Huffman frequency handling is made generic so we have
  --   one copy of the tree and of the frequency table for Encode
  --   and one for Decode

  generic
  package Huffman is
    --- Pointing parent nodes.
    --- Area [Max_Table..(Max_Table + N_CHAR - 1)] are pointers for leaves
    Parent :  array (0 .. Max_Table + N_Char - 1) of Natural;
    --- Pointing children nodes (son[], son[] + 1)
    Son   :  array (0 .. Max_Table - 1)  of Natural;

    Root_Position : constant := Max_Table - 1; -- (can be always Son'last ?)

    procedure Start;
    procedure Update_Freq_Tree (C0 : Natural);
  end Huffman;

  package body Huffman is

    Freq : array (0 .. Max_Table) of Natural; -- Cumulative freq table

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

      Freq (Freq'Last) := 16#FFFF#; -- ( Max_Table )
      Parent (Root_Position) := 0;
    end Start;

    procedure Update_Freq_Tree (C0 : Natural) is

      procedure Reconstruct_Freq_Tree is
        I, J, K, F, L : Natural;
      begin
        --  Halven cumulative freq for leaf nodes
        J := 0;
        for I in 0 .. Root_Position  loop
          if Son (I) >= Max_Table then
            Freq (J) := (Freq (I) + 1) / 2;
            Son (J) := Son (I);
            J := J + 1;
          end if;
        end loop;

        --  Make a tree : first, connect children nodes
        I := 0;
        for J in N_Char .. Root_Position  loop -- J : free nodes
          K := I + 1;
          F := Freq (I) + Freq (K); -- new frequency
          Freq (J) := F;
          K := J - 1;
          while F < Freq (K) loop
            K := K - 1;
          end loop;

          K := K + 1;
          L := J - K; -- 2007: fix: was L:= (J-K)*2, memcopy parameter remain

          Freq (K + 1 .. K + L) := Freq (K .. K + L - 1); -- shift by one cell right
          Freq (K) := F;
          Son (K + 1 .. K + L) := Son (K .. K + L - 1); -- shift by one cell right
          Son (K) := I;
          I := I + 2;
        end loop;

        --  Connect parent nodes
        for I in 0 .. Max_Table - 1  loop
          K := Son (I);
          Parent (K) := I;
          if K < Max_Table then
            Parent (K + 1) := I;
          end if;
        end loop;

      end Reconstruct_Freq_Tree;

      C, I, J, K, L : Natural;

    begin -- Update_Freq_Tree;
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

  ------------------------------------
  ------ Encoding / Compressing ------
  ------------------------------------

  procedure Encode is

    ----- Tables for encoding upper 6 bits of sliding dictionary pointer

    P_Len : constant array (0 .. 63) of Positive :=
     (3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6,
      6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8);

    P_Code : constant array (0 .. 63) of Unsigned_16 :=
     (16#00#, 16#20#, 16#30#, 16#40#, 16#50#, 16#58#, 16#60#, 16#68#,
      16#70#, 16#78#, 16#80#, 16#88#, 16#90#, 16#94#, 16#98#, 16#9C#,
      16#A0#, 16#A4#, 16#A8#, 16#AC#, 16#B0#, 16#B4#, 16#B8#, 16#BC#,
      16#C0#, 16#C2#, 16#C4#, 16#C6#, 16#C8#, 16#CA#, 16#CC#, 16#CE#,
      16#D0#, 16#D2#, 16#D4#, 16#D6#, 16#D8#, 16#DA#, 16#DC#, 16#DE#,
      16#E0#, 16#E2#, 16#E4#, 16#E6#, 16#E8#, 16#EA#, 16#EC#, 16#EE#,
      16#F0#, 16#F1#, 16#F2#, 16#F3#, 16#F4#, 16#F5#, 16#F6#, 16#F7#,
      16#F8#, 16#F9#, 16#FA#, 16#FB#, 16#FC#, 16#FD#, 16#FE#, 16#FF#);

    Putbuf : Unsigned_16 := 0;
    Putlen : Natural := 0;
    Codesize : Natural := 0;

    Node_Nil : constant := String_buffer_size;    -- End of tree's node

    Lson, Dad :  array (0 .. String_buffer_size) of Natural;
    Rson :      array (0 .. String_buffer_size + 256) of Natural;

    procedure Init_Tree is
    begin
      for  I in  String_buffer_size + 1 .. Rson'Last loop
        Rson (I) := Node_Nil;
      end loop; -- root
      for  I in  0 .. String_buffer_size - 1 loop
        Dad (I)  := Node_Nil;
      end loop; -- node
    end Init_Tree;

    Match_Position : Natural;
    Match_Length   : Natural;

    Text_Buf : Text_Buffer := empty_buffer;

    procedure Insert_Node (R : Integer) is
      I, P : Integer;
      Geq : Boolean := True;
      C :   Natural;
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

        if  I > Threshold then
          if  I > Match_Length then
            Match_Position := (R - P) mod String_buffer_size - 1;
            Match_Length := I;
            exit when Match_Length >= Look_Ahead;
          end if;
          if  I = Match_Length then
            C := (R - P) mod String_buffer_size - 1;
            if C < Match_Position then
              Match_Position := C;
            end if;
          end if;
        end if;
      end loop;

      Dad (R) := Dad (P);
      Lson (R) := Lson (P);
      Rson (R) := Rson (P);
      Dad (Lson (P)) := R;
      Dad (Rson (P)) := R;
      if Rson (Dad (P)) = P then
        Rson (Dad (P)) := R;
      else
        Lson (Dad (P)) := R;
      end if;
      Dad (P) := Node_Nil; -- remove p
    end Insert_Node;

    procedure Delete_Node (P : Natural) is
      Q : Natural;
    begin
      if Dad (P) = Node_Nil then  -- unregistered
        return;
      end if;
      if     Rson (P) = Node_Nil then
        Q := Lson (P);
      elsif  Lson (P) = Node_Nil then
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
      if  Rson (Dad (P)) = P then
        Rson (Dad (P)) := Q;
      else
        Lson (Dad (P)) := Q;
      end if;
      Dad (P) := Node_Nil;
    end Delete_Node;

    package Huffman_E is new Huffman;

    procedure Put_code (Bits_To_Output : Natural; C : Unsigned_16) is
    begin
      Putbuf := Putbuf  or  Shift_Right (C, Putlen);
      Putlen := Putlen + Bits_To_Output;
      if  Putlen >= 8 then
        Write_byte (Byte (Shift_Right (Putbuf, 8)));
        Putlen := Putlen - 8;
        if  Putlen >= 8 then
          Write_byte (Byte (Putbuf and 16#FF#));
          Codesize := Codesize + 2;
          Putlen := Putlen - 8;
          Putbuf := Shift_Left (C, Bits_To_Output - Putlen);
        else
          Putbuf := Shift_Left (Putbuf, 8);
          Codesize := Codesize + 1;
        end if;
      end if;
    end Put_code;

    procedure Encode_char (C : Natural) is
      Len, K : Natural; Code : Unsigned_16;
    begin
      Code := 0;
      Len := 0;
      K := Huffman_E.Parent (C + Max_Table);

      --  Search connections from leaf node to the root
      loop
        Code := Code / 2;
        --  If node's address is odd, output 1 else output 0
        if K mod 2 = 1 then
          Code := Code + 16#8000#;
        end if;
        Len := Len + 1;
        K := Huffman_E.Parent (K);
        exit when K = Huffman_E.Root_Position;
      end loop;

      Put_code (Len, Code);
      Huffman_E.Update_Freq_Tree (C);
    end Encode_char;

    procedure Encode_position (C : Natural) is
      I : constant Natural := C / 2**6;
    begin
      --- output upper 6 bits with encoding
      Put_code (P_Len (I), Shift_Left (P_Code (I), 8));
      --- output lower 6 bits directly
      Put_code (6, Shift_Left (Unsigned_16 (C) and  16#3F#, 10));
    end Encode_position;

    procedure Encode_end is
    begin
      if Putlen > 0 then
        Write_byte (Byte (Shift_Right (Putbuf, 8)));
        Codesize := Codesize + 1;
      end if;
      Write_byte (0); -- Write on more dummy byte
    end Encode_end;

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

    for I in 1 .. Look_Ahead loop
      Insert_Node (R - I);
    end loop;

    Insert_Node (R);

    loop
      if Match_Length > Len then
        Match_Length := Len;
      end if;
      if  Match_Length <= Threshold then
        Match_Length := 1;
        Encode_char (Natural (Text_Buf (R)));
      else
        Encode_char (255 - Threshold + Match_Length);
        Encode_position (Match_Position);
      end if;
      Last_Match_Length := Match_Length;
      I := 0;
      while I < Last_Match_Length and More_bytes loop
        I := I + 1;
        Delete_Node (S);
        C := Read_byte;
        Text_Buf (S) := C;
        if S < Look_Ahead - 1 then
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

    Encode_end;
  end Encode;

  --------------------------------------
  ------ Decoding / Uncompressing ------
  --------------------------------------

  procedure Decode is

    ----- Tables for decoding upper 6 bits of sliding dictionary pointer
    D_Code : constant array (0 .. 255) of Natural :=
     (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#,
      16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#,
      16#02#, 16#02#, 16#02#, 16#02#, 16#02#, 16#02#, 16#02#, 16#02#,
      16#02#, 16#02#, 16#02#, 16#02#, 16#02#, 16#02#, 16#02#, 16#02#,
      16#03#, 16#03#, 16#03#, 16#03#, 16#03#, 16#03#, 16#03#, 16#03#,
      16#03#, 16#03#, 16#03#, 16#03#, 16#03#, 16#03#, 16#03#, 16#03#,
      16#04#, 16#04#, 16#04#, 16#04#, 16#04#, 16#04#, 16#04#, 16#04#,
      16#05#, 16#05#, 16#05#, 16#05#, 16#05#, 16#05#, 16#05#, 16#05#,
      16#06#, 16#06#, 16#06#, 16#06#, 16#06#, 16#06#, 16#06#, 16#06#,
      16#07#, 16#07#, 16#07#, 16#07#, 16#07#, 16#07#, 16#07#, 16#07#,
      16#08#, 16#08#, 16#08#, 16#08#, 16#08#, 16#08#, 16#08#, 16#08#,
      16#09#, 16#09#, 16#09#, 16#09#, 16#09#, 16#09#, 16#09#, 16#09#,
      16#0A#, 16#0A#, 16#0A#, 16#0A#, 16#0A#, 16#0A#, 16#0A#, 16#0A#,
      16#0B#, 16#0B#, 16#0B#, 16#0B#, 16#0B#, 16#0B#, 16#0B#, 16#0B#,
      16#0C#, 16#0C#, 16#0C#, 16#0C#, 16#0D#, 16#0D#, 16#0D#, 16#0D#,
      16#0E#, 16#0E#, 16#0E#, 16#0E#, 16#0F#, 16#0F#, 16#0F#, 16#0F#,
      16#10#, 16#10#, 16#10#, 16#10#, 16#11#, 16#11#, 16#11#, 16#11#,
      16#12#, 16#12#, 16#12#, 16#12#, 16#13#, 16#13#, 16#13#, 16#13#,
      16#14#, 16#14#, 16#14#, 16#14#, 16#15#, 16#15#, 16#15#, 16#15#,
      16#16#, 16#16#, 16#16#, 16#16#, 16#17#, 16#17#, 16#17#, 16#17#,
      16#18#, 16#18#, 16#19#, 16#19#, 16#1A#, 16#1A#, 16#1B#, 16#1B#,
      16#1C#, 16#1C#, 16#1D#, 16#1D#, 16#1E#, 16#1E#, 16#1F#, 16#1F#,
      16#20#, 16#20#, 16#21#, 16#21#, 16#22#, 16#22#, 16#23#, 16#23#,
      16#24#, 16#24#, 16#25#, 16#25#, 16#26#, 16#26#, 16#27#, 16#27#,
      16#28#, 16#28#, 16#29#, 16#29#, 16#2A#, 16#2A#, 16#2B#, 16#2B#,
      16#2C#, 16#2C#, 16#2D#, 16#2D#, 16#2E#, 16#2E#, 16#2F#, 16#2F#,
      16#30#, 16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#,
      16#38#, 16#39#, 16#3A#, 16#3B#, 16#3C#, 16#3D#, 16#3E#, 16#3F#);

    D_Len : constant array (0 .. 255) of Natural :=
     (3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
      5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
      5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
      5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
      5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8);

    Getbuf : Unsigned_16 := 0;
    Getlen : Natural := 0;

    function Get_bit return  Natural is
      Result : Natural;
    begin
      while  Getlen <= 8  loop
        Getbuf := Getbuf  or  Shift_Left (Unsigned_16 (Read_byte), 8 - Getlen);
        Getlen := Getlen + 8;
      end loop;

      Result := Natural (Shift_Right (Getbuf, 15));
      Getbuf :=          Shift_Left (Getbuf, 1);
      Getlen := Getlen - 1;
      return Result;
    end Get_bit;

    function Get_decoded_position return Natural is

      function Get_byte return Natural is
        Result : Natural;
      begin
        while  Getlen <= 8  loop
          Getbuf := Getbuf  or
                    Shift_Left (Unsigned_16 (Read_byte), 8 - Getlen);
          Getlen := Getlen + 8;
        end loop;

        Result := Natural (Shift_Right (Getbuf, 8));
        Getbuf :=          Shift_Left (Getbuf, 8);
        Getlen := Getlen - 8;
        return Result;
      end Get_byte;

      I, C : Natural;
    begin
      ---decode upper 6 bits from given table
      I := Get_byte;
      C := D_Code (I) * 2**6;
      ---input lower 6 bits directly
      for J in reverse 1 .. D_Len (I) - 2 loop
        I := I * 2 + Get_bit;
      end loop;

      return C + I mod 2**6;
    end Get_decoded_position;

    package Huffman_D is new Huffman;

    function Get_decoded_char return Natural is
      C : Natural := Huffman_D.Son (Huffman_D.Root_Position);
      --  start searching tree from the root to leaves.
    begin
      --    choose node #(son[]) if input bit = 0
      --    else choose #(son[]+1) (input bit = 1)
      while  C < Max_Table  loop
        C := Huffman_D.Son (C + Get_bit);
      end loop;

      C := C - Max_Table;
      Huffman_D.Update_Freq_Tree (C);
      return C;
    end Get_decoded_char;

    I, J, R  : Natural;
    C8     : Byte;
    C      : Natural;

    Text_Buf : Text_Buffer := empty_buffer;

  begin
    if not More_bytes then
      return;
    end if;
    Huffman_D.Start;
    R := String_buffer_size - Look_Ahead;
    while More_bytes loop
      C := Get_decoded_char;
      if C < 256 then
        C8 := Unsigned_8 (C);
        Write_byte (C8);
        Text_Buf (R) := C8;
        R := (R + 1) mod String_buffer_size;
      else
        I := (R - Get_decoded_position - 1) mod String_buffer_size;
        J := C - 255 + Threshold;
        for K in 0 .. J - 1 loop
          C8 := Text_Buf ((I + K)  mod String_buffer_size);
          Write_byte (C8);
          Text_Buf (R) := C8;
          R := (R + 1) mod String_buffer_size;
        end loop;
      end if;
    end loop;
  end Decode;

end LZH;
