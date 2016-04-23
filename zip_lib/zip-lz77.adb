--  There are two LZ77 encoders at choice here:
--
--    1/  LZ77_using_LZHuf, based on LZHuf
--
--    2/  LZ77_using_IZ, based on Info-Zip's Zip's deflate.c which is
--          actually the LZ77 part of Zip's compression.
--
--  Variant 1/ is working since 2009. Two problems: it is slow and not
--     well adapted to the Deflate format (mediocre compression).
--
--  Variant 2/ is much faster, and better suited for Deflate. Added 05-Mar-2016.
--     The code is tailored and optimized for a single set of
--     the String_buffer_size, Look_Ahead, Threshold LZ77 parameters.

--  To do:
--    - LZ77 / IZ: similar to the test with TOO_FAR, try to cluster distances around
--        values needing less extra bits (may not work at all...)
--    - LZ77 / IZ: tune TOO_FAR (max: 32767), see http://optipng.sf.net/pngtech/too_far.html
--        "TOO_FAR in zlib Is Not Too Far" for discussion
--    - LZ77 (for Deflate): try to get > 258 lengths and split into 258 + something.
--        Length 258 is encoded with no extra bit, could be good...
--    - LZ77: try yet another LZ77, e.g. from 7-Zip, or program a new one with
--        hash chains etc.

procedure Zip.LZ77 is

  -----------------------
  --  LZHuf algorithm  --
  -----------------------

  procedure LZ77_using_LZHuf is
    --  Based on LZHUF by OKUMURA & YOSHIZAKI.
    --  Here the adaptive Huffman coding is thrown away:
    --  algorithm is used only to find matching patterns.

    N_Char    : constant Integer:= 256-Threshold+Look_Ahead;
    -- Character code (= 0..N_CHAR-1)
    Max_Table     : constant Integer:= N_Char*2-1;

    use Interfaces; -- Make Unsigned_* types visible

    type Text_Buffer is array ( 0..String_buffer_size+Look_Ahead-1 ) of Byte;
    empty_buffer: constant Text_Buffer:= (others=> 32); -- ' '

    -- > The Huffman frequency handling is made generic so we have
    --   one copy of the tree and of the frequency table for Encode
    --   and one for Decode

    generic
    package Huffman is
      --- Pointing parent nodes.
      --- Area [Max_Table..(Max_Table + N_CHAR - 1)] are pointers for leaves
      Parent:  array ( 0..Max_Table+N_Char-1 ) of Natural;
      --- Pointing children nodes (son[], son[] + 1)
      Son   :  array ( 0..Max_Table-1 )  of Natural;

      Root_Position : constant Integer:= Max_Table-1; -- (can be always Son'last ?)

      procedure Start;
      procedure Update_Freq_Tree( C0: Natural );
    end Huffman;

    package body Huffman is

      Freq: array ( 0..Max_Table ) of Natural; -- Cumulative freq table

      Max_Freq: constant := 16#8000#;
      -- ^-- update when cumulative frequency reaches to this value

      procedure Start is
        I: Natural;
      begin
        for J in  0 .. N_Char-1  loop
          Freq(J):= 1;
          Son (J):= J + Max_Table;
          Parent(J + Max_Table):= J;
        end loop;

        I:= 0;
        for J in N_Char .. Root_Position  loop
          Freq(J):= Freq(I)+Freq(I+1);
          Son (J):= I;
          Parent(I):= J;
          Parent(I+1):= J;
          I:= I + 2;
        end loop;

        Freq( Freq'Last ):= 16#FFFF#; -- ( Max_Table )
        Parent( Root_Position ):= 0;
      end Start;

      procedure Update_Freq_Tree( C0: Natural ) is

        procedure Reconstruct_Freq_Tree is
          I,J,K,F,L: Natural;
        begin
          -- Halven cumulative freq for leaf nodes
          J:= 0;
          for I in 0 .. Root_Position loop
            if Son(I) >= Max_Table then
              Freq(J):= (Freq(I)+1) / 2;
              Son (J):= Son(I);
              J:= J + 1;
            end if;
          end loop;

          -- Make a tree : first, connect children nodes
          I:= 0;
          for J in N_Char .. Root_Position loop -- J : free nodes
            K:= I+1;
            F:= Freq(I) + Freq(K); -- new frequency
            Freq(J):= F;
            K:= J-1;
            while F < Freq(K) loop
              K:= K-1;
            end loop;

            K:= K+1;
            L:= J-K; -- 2007: fix: was L:= (J-K)*2, memcopy parameter remain

            Freq( K+1 .. K+L ):= Freq( K .. K+L-1 ); -- shift by one cell right
            Freq(K):= F;
            Son ( K+1 .. K+L ):= Son ( K .. K+L-1 ); -- shift by one cell right
            Son (K):= I;
            I:= I + 2;
          end loop;

          -- Connect parent nodes
          for I in 0 .. Max_Table-1 loop
            K:= Son(I);
            Parent(K):= I;
            if K < Max_Table then
              Parent(K+1):= I;
            end if;
          end loop;

        end Reconstruct_Freq_Tree;

        C,I,J,K,L: Natural;

      begin -- Update_Freq_Tree;
        if Freq( Root_Position ) = Max_Freq then
          Reconstruct_Freq_Tree;
        end if;
        C:= Parent(C0 + Max_Table);
        loop
          Freq(C):= Freq(C) + 1;
          K:= Freq(C);
          -- Swap nodes to keep the tree freq-ordered
          L:= C+1;
          if  K > Freq(L) then
            while K > Freq(L+1) loop
              L:= L + 1;
            end loop;

            Freq(C):= Freq(L);
            Freq(L):= K;

            I:= Son(C);
            Parent(I):= L;
            if I < Max_Table then
              Parent(I+1):= L;
            end if;

            J:= Son(L);
            Son(L):= I;

            Parent(J):= C;
            if J < Max_Table then
              Parent(J+1):= C;
            end if;
            Son(C):= J;

            C := L;
          end if;
          C:= Parent(C);
          exit when C=0;
        end loop;        -- do it until reaching the root
      end Update_Freq_Tree;

    end Huffman;

    Node_Nil : constant Integer:= String_buffer_size;    -- End of tree's node

    Lson,Dad:  array ( 0..String_buffer_size       ) of Natural;
    Rson:      array ( 0..String_buffer_size + 256 ) of Natural;

    procedure Init_Tree is
    begin
      for I in String_buffer_size+1 .. Rson'Last loop
        Rson(I) := Node_Nil;
      end loop; -- root
      for I in 0 .. String_buffer_size-1 loop
        Dad(I)  := Node_Nil;
      end loop; -- node
    end Init_Tree;

    Match_Position : Natural;
    Match_Length   : Natural;

    Text_Buf: Text_Buffer:= empty_buffer;

    procedure Insert_Node (R: Integer) is
      I,P: Integer;
      Geq: Boolean:= True;
      C:   Natural;
    begin
      P:= String_buffer_size + 1 + Integer(Text_Buf(R));
      Rson(R):= Node_Nil;
      Lson(R):= Node_Nil;
      Match_Length := 0;
      loop
        if Geq then
          if Rson(P) = Node_Nil then
            Rson(P):= R;
            Dad(R) := P;
            return;
          end if;
          P:= Rson(P);
        else
          if Lson(P) = Node_Nil then
            Lson(P):= R;
            Dad(R) := P;
            return;
          end if;
          P:= Lson(P);
        end if;
        I:= 1;
        while I < Look_Ahead and then Text_Buf(R+I) = Text_Buf(P+I)  loop
          I:= I + 1;
        end loop;

        Geq:= Text_Buf(R+I) >= Text_Buf(P+I) or I = Look_Ahead;

        if I > Threshold then
          if I > Match_Length then
            Match_Position := (R-P) mod String_buffer_size - 1;
            Match_Length:= I;
            exit when Match_Length >= Look_Ahead;
          end if;
          if I = Match_Length then
            C:= (R-P) mod String_buffer_size - 1;
            if C < Match_Position then
              Match_Position:= C;
            end if;
          end if;
        end if;
      end loop;

      Dad (R):= Dad (P);
      Lson(R):= Lson(P);
      Rson(R):= Rson(P);
      Dad(Lson(P)):= R;
      Dad(Rson(P)):= R;
      if Rson(Dad(P)) = P then
        Rson(Dad(P)):= R;
      else
        Lson(Dad(P)):= R;
      end if;
      Dad(P):= Node_Nil; -- remove P
    end Insert_Node;

    procedure Delete_Node (P: Natural) is
      Q: Natural;
    begin
      if Dad(P) = Node_Nil then  -- unregistered
        return;
      end if;
      if    Rson(P) = Node_Nil then
        Q:= Lson(P);
      elsif Lson(P) = Node_Nil then
        Q:= Rson(P);
      else
        Q:= Lson(P);
        if Rson(Q) /= Node_Nil then
          loop
            Q:= Rson(Q);
            exit when Rson(Q) = Node_Nil;
          end loop;
          Rson(Dad(Q)):= Lson(Q);
          Dad(Lson(Q)):= Dad(Q);
          Lson(Q):= Lson(P);
          Dad(Lson(P)):= Q;
        end if;
        Rson(Q):= Rson(P);
        Dad(Rson(P)):= Q;
      end if;
      Dad(Q):= Dad(P);
      if Rson(Dad(P))=P then
        Rson(Dad(P)):= Q;
      else
        Lson(Dad(P)):= Q;
      end if;
      Dad(P):= Node_Nil;
    end Delete_Node;

    package Huffman_E is new Huffman;

    I,R,S,Last_Match_Length: Natural;
    Len: Integer;
    C: Byte;
  begin
    if not More_bytes then
      return;
    end if;
    Huffman_E.Start;
    Init_Tree;
    S:= 0;
    R:= String_buffer_size - Look_Ahead;
    Len:= 0;
    while Len < Look_Ahead and More_bytes loop
      Text_Buf(R+Len):= Read_byte;
      Len:= Len + 1;
    end loop;

    --  Seems: fill dictionary with default value
    --
    --  for I in 1.. Look_Ahead loop
    --    Insert_Node(R - I);
    --  end loop;

    Insert_Node(R);

    loop
      if Match_Length > Len then
        Match_Length:= Len;
      end if;
      if Match_Length <= Threshold then
        Match_Length := 1;
        Huffman_E.Update_Freq_Tree( Natural(Text_Buf(R)) );
        Write_byte( Text_Buf(R) );
      else
        Write_code(Match_Position+1, Match_Length);
      end if;
      Last_Match_Length := Match_Length;
      I:= 0;
      while I < Last_Match_Length and More_bytes loop
        I:= I + 1;
        Delete_Node(S);
        C:= Read_byte;
        Text_Buf(S):= C;
        if  S < Look_Ahead-1 then
          Text_Buf(S+String_buffer_size):= C;
        end if;
        S:= (S+1) mod String_buffer_size;
        R:= (R+1) mod String_buffer_size;
        Insert_Node(R);
      end loop;

      while I < Last_Match_Length loop
        I:= I + 1;
        Delete_Node(S);
        S := (S+1) mod String_buffer_size;
        R := (R+1) mod String_buffer_size;
        Len:= Len - 1;
        if Len > 0 then
          Insert_Node(R);
        end if;
      end loop;

      exit when Len=0;
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

  procedure LZ77_using_IZ(level: Natural) is
    HASH_BITS: constant:= 15;  --  13..15
    HASH_SIZE: constant:= 2 ** HASH_BITS;
    HASH_MASK: constant:= HASH_SIZE - 1;
    WSIZE    : constant Integer_M32:= Integer_M32(String_buffer_size);
    WMASK    : constant Unsigned_M16:= Unsigned_M16(WSIZE - 1);
    --  HASH_SIZE and WSIZE must be powers of two
    NIL      : constant:= 0;  --  Tail of hash chains
    TOO_FAR  : constant:= 4096;  --  Matches of length 3 are discarded if their distance exceeds TOO_FAR
    --
    use Interfaces;
    subtype ulg is Unsigned_M32;
    subtype unsigned is Unsigned_M16;
    subtype ush is Unsigned_M16;
    --  subtype long is Integer_M32;
    --  subtype int is Integer;
    subtype Pos is Unsigned_M32;  --  must be at least 32 bits
    --  subtype IPos is unsigned;
    --  A Pos is an index in the character window. IPos is used only for parameter passing.
    window: array(0 .. 2 * WSIZE - 1) of Byte;
    --  Sliding window. Input bytes are read into the second half of the window,
    --  and move to the first half later to keep a dictionary of at least WSIZE
    --  bytes. With this organization, matches are limited to a distance of
    --  WSIZE-MAX_MATCH bytes, but this ensures that IO is always
    --  performed with a length multiple of the block size.
    prev: array(0..unsigned(WSIZE - 1)) of Pos;
    --  Link to older string with same hash index.
    --  This link is maintained only for the last 32K strings.
    --  An index in this array is thus a window index modulo 32K.
    head: array(0..unsigned(HASH_SIZE - 1)) of Pos;
    --  Heads of the hash chains or NIL.
    window_size: ulg;
    --  window size, 2*WSIZE except for MMAP or BIG_MEM, where it is the
    --  input file length plus MIN_LOOKAHEAD.
    sliding: Boolean;  --  Set to False when the input file is already in memory  [was: int]
    ins_h: unsigned;   --  hash index of string to be inserted
    MIN_MATCH: constant Integer_M32:= Integer_M32(Threshold) + 1;  --  Deflate: 3
    MAX_MATCH: constant Integer_M32:= Integer_M32(Look_Ahead);     --  Deflate: 258
    MIN_LOOKAHEAD: constant Integer_M32:= MAX_MATCH + MIN_MATCH + 1;
    --  Minimum amount of lookahead, except at the end of the input file.
    MAX_DIST : constant Integer_M32:= WSIZE - MIN_LOOKAHEAD;
    H_SHIFT: constant Integer:= Integer((HASH_BITS + MIN_MATCH - 1) / MIN_MATCH);
    --  Number of bits by which ins_h and del_h must be shifted at each
    --  input step. It must be such that after MIN_MATCH steps, the oldest
    --  byte no longer takes part in the hash key, that is:
    --  H_SHIFT * MIN_MATCH >= HASH_BITS
    prev_length: Natural_M32; --  [was: unsigned]
    --  Length of the best match at previous step. Matches not greater than this
    --  are discarded. This is used in the lazy match evaluation.
    strstart   : Natural_M32;   --  start of string to insert [was: unsigned]
    match_start: Natural_M32;   --  start of matching string [was: unsigned]
    eofile     : Boolean;       --  flag set at end of input file [was: int]
    lookahead  : Natural_M32;   --  number of valid bytes ahead in window  [was: unsigned]
    max_chain_length : unsigned;
    --  To speed up deflation, hash chains are never searched beyond this length.
    --  A higher limit improves compression ratio but degrades the speed.
    max_lazy_match: Natural_M32;  --  [was: unsigned]
    --  Attempt to find a better match only when the current match is strictly
    --  smaller than this value. This mechanism is used only for compression
    --  levels >= 4.
    good_match: Natural_M32;  --  [was: unsigned]
    --  Use a faster search when the previous match is longer than this
    nice_match: Integer_M32;  --  Stop searching when current match exceeds this
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

    configuration_table: constant array(0..10) of config:= (
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

    procedure UPDATE_HASH(h: in out unsigned; c: Byte) is
    pragma Inline(UPDATE_HASH);
    begin
      h := (unsigned(Shift_Left(Unsigned_32(h), H_SHIFT)) xor unsigned(c)) and HASH_MASK;
    end UPDATE_HASH;

    --  Insert string starting at s in the dictionary and set match_head to the previous head
    --  of the hash chain (the most recent string with same hash key). Return
    --  the previous length of the hash chain.
    --  IN  assertion: all calls to to INSERT_STRING are made with consecutive
    --     input characters and the first MIN_MATCH bytes of s are valid
    --     (except for the last MIN_MATCH-1 bytes of the input file).

    procedure INSERT_STRING(s: Integer_M32; match_head: out Natural_M32) is
    pragma Inline(INSERT_STRING);
    begin
      UPDATE_HASH(ins_h, window(s + MIN_MATCH - 1));
      match_head := Natural_M32(head(ins_h));
      prev(unsigned(s) and WMASK):= Pos(match_head);
      head(ins_h) := Pos(s);
    end INSERT_STRING;

    procedure Read_buf(from: Integer_M32; amount: unsigned; actual: out Integer_M32) is
      need: unsigned:= amount;
    begin
      --  put_line("Read buffer: from:" & from'img & ";  amount:" & amount'img);
      actual:= 0;
      while need > 0 and then More_bytes loop
        window(from + actual):= Read_byte;
        actual:= actual + 1;
        need:= need - 1;
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
      more: unsigned;
      m: Pos;
      n: Natural_M32;
    begin
      loop
        more:= unsigned(window_size - ulg(lookahead) - ulg(strstart));
        if False then  --  C: "if (more == (unsigned)EOF) {" ?... GdM: seems a 16-bit code for EOF
          --  Very unlikely, but possible on 16 bit machine if strstart == 0
          --  and lookahead == 1 (input done one byte at time)
          more:= more - 1;
        elsif strstart >= WSIZE + MAX_DIST and then sliding then
          --  By the IN assertion, the window is not empty so we can't confuse
          --  more == 0 with more == 64K on a 16 bit machine.
          window(0 .. WSIZE - 1):= window(WSIZE .. 2 * WSIZE - 1);
          --  GdM: in rare cases (e.g. level 9 on test file "enwik8"), match_start happens
          --  to be < WSIZE. We do as in the original 16-bit C code: mod 2**16, such that the
          --  index is the window's range.
          --  This assumes WSIZE = 2**15, which is checked at startup of LZ77_using_IZ.
          --  Very likely, match_start is garbage anyway - see http://sf.net/p/infozip/bugs/49/
          match_start := Natural_M32( Unsigned_16(match_start) - Unsigned_16(WSIZE) );
          strstart    := strstart - WSIZE; -- we now have strstart >= MAX_DIST:
          for nn in 0 .. unsigned'(HASH_SIZE - 1) loop
            m := head(nn);
            if m >= Pos(WSIZE) then
              head(nn) := m - Pos(WSIZE);
            else
              head(nn) := NIL;
            end if;
          end loop;
          --
          for nn in 0 .. unsigned(WSIZE - 1) loop
            m := prev(nn);
            if m >= Pos(WSIZE) then
              prev(nn) := m - Pos(WSIZE);
            else
              prev(nn) := NIL;
            end if;
            --  If n is not on any hash chain, prev[n] is garbage but its value will never be used.
          end loop;
          more:= more + unsigned(WSIZE);
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
        Read_buf(strstart + lookahead, more, n);
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

    procedure LM_Init (pack_level: Natural) is
    begin
      --  Do not slide the window if the whole input is already in memory (window_size > 0)
      sliding := False;
      if window_size = 0 then
        sliding := True;
        window_size := 2 * ulg(WSIZE);
      end if;
      --  Initialize the hash table.
      --  prev will be initialized on the fly.
      head:= (others => NIL);
      --  Set the default configuration parameters:
      max_lazy_match   := configuration_table(pack_level).max_lazy;
      good_match       := configuration_table(pack_level).good_length;
      nice_match       := configuration_table(pack_level).nice_length;
      max_chain_length := configuration_table(pack_level).max_chain;
      --  Info-Zip comment: ??? reduce max_chain_length for binary files
      strstart := 0;
      Read_buf(0, unsigned(WSIZE), lookahead);
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
      for j in 0 .. Natural_M32(MIN_MATCH)-2 loop
        UPDATE_HASH(ins_h, window(j));
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

    procedure Longest_Match(current_match: in out Integer_M32; longest: out Integer_M32) is
      chain_length : unsigned := max_chain_length;  --  max hash chain length
      scan         : Integer_M32 := strstart;       --  current string
      match        : Integer_M32;                   --  matched string
      len          : Integer_M32;                   --  length of current match
      best_len     : Integer_M32 := prev_length;    --  best match length so far
      limit        : Natural_M32;  --  [was: IPos]
      strend       : constant Integer_M32:= strstart + MAX_MATCH;
      scan_end     : Integer_M32:= scan + best_len;
    begin
      --  Stop when current_match becomes <= limit. To simplify the code,
      --  we prevent matches with the string of window index 0.
      if strstart > MAX_DIST then
        limit:= strstart - MAX_DIST;
      else
        limit:= NIL;
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
        if window(match + best_len)     /= window(scan_end) or else
           window(match + best_len - 1) /= window(scan_end - 1) or else
           window(match)                /= window(scan) or else
           window(match + 1)            /= window(scan + 1)
        then
          match:= match + 1;  --  C: continue
        else
          --  The check at best_len - 1 can be removed because it will be made
          --  again later. (This heuristic is not always a win.)
          --
          --  It is not necessary to compare window(scan + 2) and window(match + 2) since they
          --  are always equal when the other bytes match, given that
          --  the hash keys are equal and that HASH_BITS >= 8.
          scan:= scan + 2;
          match:= match + 2;
          --  C: The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
          --     It is easy to get rid of this optimization if necessary.
          --  Ada: see the "else" part below.
          if MAX_MATCH = 258 then
            --  We check for insufficient lookahead only every 8th comparison;
            --  the 256th check will be made at strstart + 258.
            loop
              scan:= scan + 1;
              match:= match + 1;
              exit when window(scan) /= window(match);
              scan:= scan + 1;
              match:= match + 1;
              exit when window(scan) /= window(match);
              scan:= scan + 1;
              match:= match + 1;
              exit when window(scan) /= window(match);
              scan:= scan + 1;
              match:= match + 1;
              exit when window(scan) /= window(match);
              scan:= scan + 1;
              match:= match + 1;
              exit when window(scan) /= window(match);
              scan:= scan + 1;
              match:= match + 1;
              exit when window(scan) /= window(match);
              scan:= scan + 1;
              match:= match + 1;
              exit when window(scan) /= window(match);
              scan:= scan + 1;
              match:= match + 1;
              exit when window(scan) /= window(match) or else scan >= strend;
            end loop;
          else
            --  We check for insufficient lookahead after every comparison.
            loop
              scan:= scan + 1;
              match:= match + 1;
              exit when window(scan) /= window(match) or else scan >= strend;
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
        current_match := Integer_M32(prev(unsigned(current_match) and WMASK));
        exit when current_match <= limit;
        chain_length:= chain_length - 1;
        exit when chain_length = 0;
      end loop;
      longest:= best_len;
    end Longest_Match;

    procedure LZ77_part_of_IZ_Deflate is
      hash_head : Natural_M32:= NIL;              --  head of hash chain
      prev_match: Natural_M32;                    --  previous match  [was: IPos]
      match_available: Boolean:= False;           --  set if previous match exists
      match_length: Natural_M32:= MIN_MATCH - 1;  --  length of best match
      max_insert: Natural_M32;
    begin
      match_start:= 0;  --  NB: no initialization in deflate.c
      --  NB: level <= 3 would call deflate_fast;
      --
      --  Process the input block.
      while lookahead /= 0 loop
        --  Insert the string window(strstart .. strstart + 2) in the
        --  dictionary, and set hash_head to the head of the hash chain:
        if lookahead >= MIN_MATCH then
          INSERT_STRING(strstart, hash_head);
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
          Longest_Match(hash_head, match_length);
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
          max_insert:= strstart + lookahead - MIN_MATCH;
          --  C: in DEBUG mode: check_match(strstart-1, prev_match, prev_length);
          --
          ------------------------------------
          --  Output a Distance-Length code --
          ------------------------------------
          Write_code(Positive(strstart - 1 - prev_match), Positive(prev_length));
          --  Insert in hash table all strings up to the end of the match.
          --  strstart-1 and strstart are already inserted.
          lookahead := lookahead - (prev_length-1);
          prev_length := prev_length - 2;
          loop
            strstart:= strstart + 1;
            if strstart <= max_insert then
              INSERT_STRING(strstart, hash_head);
              --  strstart never exceeds WSIZE - MAX_MATCH, so there
              --  are always MIN_MATCH bytes ahead.
            end if;
            prev_length:= prev_length - 1;
            exit when prev_length = 0;
          end loop;
          strstart:= strstart + 1;
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
          Write_byte(window(strstart-1));
          strstart:= strstart + 1;
          lookahead := lookahead - 1;
        else
          --  There is no previous match to compare with, wait for the next step to decide.
          match_available := True;
          strstart:= strstart + 1;
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
        Write_byte(window(strstart-1));
      end if;
    end LZ77_part_of_IZ_Deflate;

    Code_too_clever: exception;
  begin
    if Look_Ahead /= 258 or String_buffer_size /= 2 ** 15 or Threshold /= 2 then
      raise Code_too_clever;  --  was optimized for these parameters
    end if;
    window_size:= 0;
    LM_Init(level);
    LZ77_part_of_IZ_Deflate;
  end LZ77_using_IZ;

begin
  case Method is
    when LZHuf =>
      LZ77_using_LZHuf;
    when IZ_4 .. IZ_10 =>
      LZ77_using_IZ( 4 + LZ77_method'Pos(Method) -  LZ77_method'Pos(IZ_4) );
  end case;
end Zip.LZ77;
