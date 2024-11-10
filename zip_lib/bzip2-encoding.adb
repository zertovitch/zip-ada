--  BZip2.Encoding - a standalone, generic BZip2 encoder.
------------------
--
--  Examples of use:
--    BZip2_Enc, a standalone encoder to .bz2 files
--    Zip.Compress.BZip2_E, creates Zip files entries with BZip2 encoding

--  Legal licensing note:

--  Copyright (c) 2024 Gautier de Montmollin
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

with Huffman.Encoding.Length_Limited_Coding;

with Ada.Containers.Generic_Constrained_Array_Sort,
     Ada.Strings.Unbounded,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

package body BZip2.Encoding is

  procedure Encode (option : Compression_Option := block_900k) is
    use Interfaces;

    subtype Bit_Pos_Type is Natural range 0 .. 7;
    bit_buffer : Byte := 0;
    bit_pos : Bit_Pos_Type := 7;

    procedure Flush_Bit_Buffer is
    begin
      Write_Byte (bit_buffer);
      bit_buffer := 0;
      bit_pos := 7;
    end Flush_Bit_Buffer;

    procedure Put_Bits (data : Unsigned_32; amount : Positive) is
    begin
      for count in reverse 1 .. amount loop
        if (data and Shift_Left (Unsigned_32'(1), count - 1)) /= 0 then
          bit_buffer := bit_buffer or Shift_Left (Unsigned_8'(1), bit_pos);
        end if;
        if bit_pos = 0 then
          Flush_Bit_Buffer;
        else
          bit_pos := bit_pos - 1;
        end if;
      end loop;
    end Put_Bits;

    procedure Put_Bits (b : Boolean) is
    begin
      Put_Bits (Boolean'Pos (b), 1);
    end Put_Bits;

    procedure Put_Bits (s : String) is
    begin
      for c of s loop
        Put_Bits (Character'Pos (c), 8);
      end loop;
    end Put_Bits;

    level : constant Natural_32 :=
      (case option is
         when block_50k  => 1,
         when block_100k => 1,
         when block_400k => 4,
         when block_900k => 9);

    block_capacity : constant Natural_32 :=
      (case option is
         when block_50k => sub_block_size / 2,
         when others    => sub_block_size * level);

    type Buffer is array (Natural_32 range <>) of Byte;
    type Buffer_Access is access Buffer;

    procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Buffer, Buffer_Access);

    data : Buffer_Access;

    combined_crc : Unsigned_32 := 0;

    block_counter : Natural := 0;

    --  Each block is limited either by the data available
    --  (More_Bytes = False) or by the block capacity.
    --  It means that each encoding step has an end and
    --  that we can theoretically go on with the next step,
    --  perhaps at the price of using more memory.

    procedure Encode_Block is

      quiet          : constant := 0;
      headlines      : constant := 1;
      detailed       : constant := 2;
      super_detailed : constant := 3;  --  Details down to symbols.

      verbosity_level : constant := quiet;

      procedure Trace (msg : String; verbosity : Natural) with Inline is
      begin
        if verbosity_level >= verbosity then
          Ada.Text_IO.Put_Line ("BZip2: " & msg);
        end if;
      end Trace;

      procedure Trace (prefix : String; b : Buffer; verbosity : Natural) with Inline is
      begin
        if verbosity_level >= verbosity then
          declare
            use Ada.Strings.Unbounded;
            msg : Unbounded_String;
          begin
            for bt of b loop
              if bt in 32 .. 126 then
                msg := msg & Character'Val (bt);
              else
                msg := msg & '(' & bt'Image & ')';
              end if;
            end loop;
            Trace (prefix & To_String (msg), verbosity);
          end;
        end if;
      end Trace;

      -----------------------------------
      --  Initial Run-Length Encoding  --
      -----------------------------------

      block_size : Natural_32 := 0;
      block_crc : Unsigned_32;
      in_use : array (Byte) of Boolean := (others => False);

      procedure RLE_1 is

        procedure Store (x : Byte) with Inline is
        begin
          block_size := block_size + 1;
          data (block_size) := x;
          in_use (x) := True;
        end Store;

        b : Byte;
        b_prev : Byte := 0;  --  Initialization is to reassure the compiler.
        run : Natural := 0;

        procedure Store_Run with Inline is
        begin
          for count in 1 .. Integer'Min (4, run) loop
            Store (b_prev);
          end loop;
          if run >= 4 then
            pragma Assert (run <= 259);
            Store (Byte (run - 4));
          end if;
          run := 1;
        end Store_Run;

        start : Boolean := True;
      begin
        CRC.Init (block_crc);
        while block_size + 5 < block_capacity and then More_Bytes loop
          --  ^ The +5 is because sometimes a pack of max 5 bytes is sent by Store_Run.
          b := Read_Byte;
          CRC.Update (block_crc, b);
          if start or else b /= b_prev then
            --  Startup or Run break:
            Store_Run;
            start := False;
          elsif run = 259 then
            --  Force a run break, even though b = b_prev:
            Store_Run;
          else
            run := run + 1;
          end if;
          b_prev := b;
        end loop;
        Store_Run;
        if verbosity_level >= super_detailed then
          Trace ("RLE_1: ", data (1 .. block_size), super_detailed);
        end if;
      end RLE_1;

      ---------------------------------
      --  Burrows-Wheeler Transform  --
      ---------------------------------

      bwt_data : Buffer_Access;
      bwt_index : Natural_32 := 0;  --  0-based.

      procedure BWT is

        subtype Offset_Range is Integer_32 range 0 .. block_size - 1;

        type Offset_Table is array (Offset_Range) of Offset_Range;
        type Offset_Table_Access is access Offset_Table;

        procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Offset_Table, Offset_Table_Access);

        --  Compare the message, rotated with two (possibly different) offsets.
        function Lexicographically_Smaller (left, right : Offset_Range) return Boolean with Inline is
          l, r : Byte;
        begin
          for i in Offset_Range loop
            l := data (data'First + (i - left)  mod block_size);
            r := data (data'First + (i - right) mod block_size);
            if l < r then
              return True;
            elsif l > r then
              return False;
            end if;
          end loop;
          --  Equality.
          return False;
        end Lexicographically_Smaller;

       procedure Offset_Sort is new Ada.Containers.Generic_Constrained_Array_Sort
         (Index_Type   => Offset_Range,
          Element_Type => Offset_Range,
          Array_Type   => Offset_Table,
          "<"          => Lexicographically_Smaller);

         offset : Offset_Table_Access := new Offset_Table;

      begin
        for i in Offset_Range loop
          offset (i) := i;
        end loop;

        Offset_Sort (offset.all);  --  <--- The BW Transform is done here.

        bwt_data := new Buffer (1 .. block_size);
        for i in Offset_Range loop
          --  Copy last column of the matrix into transformed message:
          bwt_data (1 + i) := data (1 + (block_size - 1 - offset (i)) mod block_size);
          if offset (i) = 0 then
            --  Found the row index of the original message.
            bwt_index := i;
          end if;
        end loop;

        if verbosity_level >= super_detailed then
          if block_size = 0 then
            Trace ("BWT:   emtpy block", super_detailed);
          else
            Trace ("BWT:   ", bwt_data.all, super_detailed);
            Trace ("BWT index:" & bwt_index'Image, super_detailed);
          end if;
        end if;
        Unchecked_Free (offset);
      end BWT;

      ----------------------------------------------------
      --  Move-to-Front and second Run-Length Encoding  --
      ----------------------------------------------------

      subtype Max_Alphabet is Integer range 0 .. max_alphabet_size - 1;

      type MTF_Array is array (Positive_32 range <>) of Max_Alphabet;
      type MTF_Array_Access is access MTF_Array;

      mtf_data : MTF_Array_Access;
      mtf_last : Natural_32 := 0;

      unseq_to_seq : array (Byte) of Byte;

      normal_symbols_in_use : Natural;
      last_symbol_in_use : Natural;
      EOB : Natural;

      procedure MTF_and_RLE_2 is

        procedure Prepare_Mapping is
        begin
          normal_symbols_in_use := 0;
          for i in Byte loop
            if in_use (i) then
              unseq_to_seq (i) := Byte (normal_symbols_in_use);
              normal_symbols_in_use := normal_symbols_in_use + 1;
            end if;
          end loop;
          last_symbol_in_use := normal_symbols_in_use + 3 - 1 - 1;
          --  + 3 : the special symbols RUN_A, RUN_B, EOB
          --  - 1 : value 0 has no symbol (RUN_A and RUN_B are used for the runs of 0)
          --  - 1 : zero-based

          EOB := last_symbol_in_use;

          Trace ("Normal symbols in use:" & normal_symbols_in_use'Image, detailed);
        end Prepare_Mapping;

        procedure Store (a : Max_Alphabet) is
        begin
          mtf_last := mtf_last + 1;
          mtf_data (mtf_last) := a;
        end Store;

        run : Natural_32 := 0;

        procedure Store_Run with Inline is
        begin
          if run > 0 then
            declare
              rc : Unsigned_32 := Unsigned_32 (run + 1);
            begin
              loop
                Store (Max_Alphabet (rc and 1));  --  Emit run_a or run_b.
                rc := Shift_Right (rc, 1);
                exit when rc < 2;
              end loop;
            end;
            run := 0;
          end if;
        end Store_Run;

        mtf_symbol : array (0 .. 255) of Byte;
        idx : Natural;
        bt_seq : Byte;
      begin
        Prepare_Mapping;

        mtf_data := new MTF_Array (1 .. 1 + 2 * block_size);  --  Check real worst case capacity !!

        for i in mtf_symbol'Range loop
          mtf_symbol (i) := Byte (i);
        end loop;

        Big_MTF_RLE_2_Loop :
        for bt of bwt_data.all loop
          bt_seq := unseq_to_seq (bt);

          --  MTF part:

          Search_Value :
          for search in mtf_symbol'Range loop
            if mtf_symbol (search) = bt_seq then
              idx := search;
              exit Search_Value;
            end if;
          end loop Search_Value;

          Move_Value_to_Front :
          for i in reverse 1 .. idx loop
            mtf_symbol (i) := mtf_symbol (i - 1);
          end loop Move_Value_to_Front;
          mtf_symbol (0) := bt_seq;

          --  RLE part:

          if idx = 0 then
            run := run + 1;
          else
            Store_Run;
            Store (1 + idx);  --  Value stored is >= 2.
          end if;

        end loop Big_MTF_RLE_2_Loop;

        Store_Run;
        Store (EOB);

        Unchecked_Free (bwt_data);
      end MTF_and_RLE_2;

      -----------------------------------------------------------
      --  Entropy Encoding and output of the compressed block  --
      -----------------------------------------------------------

      subtype Entropy_Coder_Range is Integer range 1 .. max_entropy_coders;

      descr :
        array (Entropy_Coder_Range) of
          Huffman.Encoding.Descriptor (Max_Alphabet);

      entropy_coder_count : Entropy_Coder_Range;
      selector_count : Integer_32;

      selector : array (1 .. 1 + block_capacity / group_size) of Entropy_Coder_Range;

      procedure Entropy_Calculations is

        subtype Alphabet_in_Use is Integer range 0 .. last_symbol_in_use;

        type Huffman_Length_Array is array (Alphabet_in_Use) of Natural;

        type Count_Array is array (Alphabet_in_Use) of Natural_32;

        procedure Avoid_Zeros (freq : in out Count_Array) is
        begin
          for a in Alphabet_in_Use loop
            if freq (a) = 0 then
              --  Tweak the stats to avoid zeros...
              for aa in Alphabet_in_Use loop
                --  Inrease each count by 1 to avoid 0 lengths
                --  (the canonical BZip2 wants that).
                freq (aa) := freq (aa) + 1;
              end loop;
              return;
            end if;
          end loop;
        end Avoid_Zeros;

        max_code_len_agreed : constant := max_code_len_bzip2_1_0_3;

        procedure LLHCL is new
          Huffman.Encoding.Length_Limited_Coding
            (Alphabet     => Alphabet_in_Use,
             Count_Type   => Natural_32,
             Count_Array  => Count_Array,
             Length_Array => Huffman_Length_Array,
             max_bits     => max_code_len_agreed);

        trace_frequency_matrix : constant Boolean := False;

        procedure Output_Frequency_Matrix is
          use Ada.Text_IO;
          f : File_Type;
          file_name : String := "freq" & block_counter'Image & ".csv";
          freq : Count_Array := (others => 0);
          symbol : Alphabet_in_Use;
          sep : constant Character := ';';
        begin
          --  In this file, rows represent groups of data,
          --  columns represent the frequencies of each symbol.
          file_name (file_name'First + 4) := '_';
          Create (f, Out_File, file_name);
          for mtf_idx in 1 .. mtf_last loop
            symbol := mtf_data (mtf_idx);
            freq (symbol) := freq (symbol) + 1;
            if mtf_idx rem group_size = 0 or else mtf_idx = mtf_last then
              --  Dump group's statistics:
              for s in Alphabet_in_Use loop
                Put (f, freq (s)'Image & sep);
              end loop;
              New_Line (f);
              freq := (others => 0);
            end if;
          end loop;
          Close (f);
        end Output_Frequency_Matrix;

        procedure Define_Descriptor (freq : in out Count_Array; des : Entropy_Coder_Range) is
          len : Huffman_Length_Array;
        begin
          Avoid_Zeros (freq);
          LLHCL (freq, len);
          for symbol in Alphabet_in_Use loop
            descr (des)(symbol).bit_length := len (symbol);
          end loop;
          Huffman.Encoding.Prepare_Codes
            (descr (des)(Alphabet_in_Use), max_code_len_agreed, False);
        end Define_Descriptor;

        procedure Single_Entropy_Coder is
          freq : Count_Array := (others => 0);
        begin
          for symbol of mtf_data (1 .. mtf_last) loop
            freq (symbol) := freq (symbol) + 1;
          end loop;
          Define_Descriptor (freq, 1);
          entropy_coder_count := 2;  --  The canonical BZip2 decoder requires >= 2 coders.
          descr (2) := descr (1);    --  We actually don't use the copy (psssht), but need to define it!
          for i in 1 .. selector_count loop
            selector (Integer_32 (i)) := 1;
          end loop;
        end Single_Entropy_Coder;

        procedure Multiple_Entropy_Coders is

          subtype Selector_Range is Positive_32 range 1 .. selector_count;

          --  We create a ranking using the first symbols (RUN_A, RUN_B, ...).
          --  Looke at outputs of Output_Frequency_Matrix to find why.
          type Pair is record
            key   : Natural_32;   --  Occurrences of interesting symbols.
            index : Positive_32;  --  Group number.
          end record;

          type Ranking_Array is array (Selector_Range) of Pair;

          ranking : Ranking_Array;

          function Smaller_Key (left, right : Pair) return Boolean is (left.key < right.key);

          procedure Ranking_Sort is new Ada.Containers.Generic_Constrained_Array_Sort
            (Index_Type   => Selector_Range,
             Element_Type => Pair,
             Array_Type   => Ranking_Array,
             "<"          => Smaller_Key);

          --  Frequencies grouped by selected entropy coder.
          freq_cluster : array (Entropy_Coder_Range) of Count_Array;

          defectors : Natural;

          procedure Define_Simulate_and_Reclassify is
            pos_countdown : Natural := group_size;
            sel_idx : Positive_32 := 1;
            symbol : Alphabet_in_Use;
            cluster : Entropy_Coder_Range;
            bits : array (1 .. entropy_coder_count) of Natural := (others => 0);

            procedure Optimize_Group is
              min_bits : Natural := Natural'Last;
              best : Entropy_Coder_Range := cluster;
              cost : Natural;
            begin
              --  Now we have the costs in bits for the whole group
              --  of data.
              for cl in 1 .. entropy_coder_count loop
                cost := bits (cl);
                if sel_idx > 1 and then selector (sel_idx - 1) /= cl then
                  --  Here we account the mtf encoding of the selectors.
                  --  !! Approximate (only change/no change)...
                  cost := cost + 1;
                end if;
                if cost < min_bits then
                  min_bits := cost;
                  best := cl;
                end if;
              end loop;
              if best /= cluster then
                --  We have found a cheaper encoding.
                --  -> the group #sel_idx changes party.
                selector (sel_idx) := best;
                defectors := defectors + 1;
              end if;
            end Optimize_Group;

          begin
            --  Populate the frequency stats grouped by entropy coder cluster.
            --
            freq_cluster := (others => (others => 0));
            for mtf_idx in 1 .. mtf_last loop
              cluster := selector (sel_idx);
              symbol := mtf_data (mtf_idx);
              freq_cluster (cluster)(symbol) := freq_cluster (cluster)(symbol) + 1;
              pos_countdown := pos_countdown - 1;
              if pos_countdown = 0 then
                pos_countdown := group_size;
                sel_idx := sel_idx + 1;
              end if;
            end loop;

            for cl in 1 .. entropy_coder_count loop
              Define_Descriptor (freq_cluster (cl), cl);
            end loop;

            --  Cost analysis and re-classification
            --
            pos_countdown := group_size;
            sel_idx := 1;
            for mtf_idx in 1 .. mtf_last loop
              cluster := selector (sel_idx);
              symbol := mtf_data (mtf_idx);
              for cl in 1 .. entropy_coder_count loop
                 --  Simulate output assuming the current group
                 --  belongs to cluster cl.
                 bits (cl) := bits (cl) + descr (cl) (symbol).bit_length;
              end loop;
              pos_countdown := pos_countdown - 1;
              if pos_countdown = 0 then
                Optimize_Group;
                bits := (others => 0);
                pos_countdown := group_size;
                sel_idx := sel_idx + 1;
              end if;
            end loop;
            if pos_countdown < group_size then
              --  Optimize last, incomplete group.
              Optimize_Group;
            end if;
          end Define_Simulate_and_Reclassify;

          procedure Do_Ranking is
            pos_countdown : Natural := group_size;
            sel_idx : Positive_32 := 1;
            key : Natural_32 := 0;
            symbol : Alphabet_in_Use;
          begin
            --  Populate the frequency stats grouped by data group.
            --
            for mtf_idx in 1 .. mtf_last loop
              symbol := mtf_data (mtf_idx);
              --  Establish a key using the first few columns of the frequency
              --  matrix (see files generated by Output_Frequency_Matrix):
              if symbol in run_a .. Integer'Min (EOB - 1, run_a + 3) then
                key := key + 1;
              end if;
              pos_countdown := pos_countdown - 1;
              if pos_countdown = 0 then
                ranking (sel_idx) := (key => key, index => sel_idx);
                pos_countdown := group_size;
                sel_idx := sel_idx + 1;
                key := 0;
              end if;
            end loop;
            if pos_countdown < group_size then
              --  Finish last, incomplete group.
              ranking (sel_idx) := (key => key, index => sel_idx);
            end if;

            --  The construction of initial clusters can be done easily
            --  using the following sorting:
            --
            Ranking_Sort (ranking);

          end Do_Ranking;

          type Cluster_Attribution is array (Positive range <>) of Entropy_Coder_Range;

          procedure Initial_Clustering (attr : Cluster_Attribution) is
            ne : constant Positive_32 := Positive_32 (entropy_coder_count);
            ns : constant Positive_32 := selector_count;
            e32 : Positive_32;
          begin
            for e in 1 .. entropy_coder_count loop
              e32 := Positive_32 (e);
              for i in 1 + (e32 - 1) * ns / ne .. e32 * ns / ne loop
                selector (ranking (i).index) := attr (e + 1 - attr'First);
              end loop;
            end loop;
          end Initial_Clustering;

          factor : constant := 50;

        begin
          --  Entropy coder counts depending on empirical sizes.
          --  Magic numbers as in compress.c, factor is ours.
          --
          if mtf_last < 200 * factor then
            entropy_coder_count := 2;
          elsif mtf_last < 600 * factor then
            entropy_coder_count := 3;
          elsif mtf_last < 1200 * factor then
            entropy_coder_count := 4;
          elsif mtf_last < 2400 * factor then
            entropy_coder_count := 5;
          else
            entropy_coder_count := 6;
          end if;

          Do_Ranking;

          --  Create an initial clustering depending on a mix
          --  of RUN_A, RUN_B, low-index MTF occurrences.
          --  Example with two clusters:
          --   - Low values (more random data) for the cluster #2.
          --   - High values (more redundant data) for #1.
          --
          case entropy_coder_count is
            when 1 => null;  --  Not supported by canonical BZip2.
            when 2 => Initial_Clustering ((2, 1));
            when 3 => Initial_Clustering ((3, 1, 2));
            when 4 => Initial_Clustering ((4, 2, 1, 3));
            when 5 => Initial_Clustering ((5, 3, 1, 2, 4));
            when 6 => Initial_Clustering ((6, 4, 2, 1, 3, 5));
          end case;

          for iteration in 1 .. 6 loop
            defectors := 0;
            Define_Simulate_and_Reclassify;
            Trace ("Defectors:" & defectors'Image, detailed);
            exit when defectors = 0;
          end loop;

        end Multiple_Entropy_Coders;

      begin
        selector_count := 1 + (mtf_last - 1) / group_size;
        if trace_frequency_matrix then
          Output_Frequency_Matrix;
        end if;

        if mtf_last < 100 then
          Single_Entropy_Coder;
        else
          Multiple_Entropy_Coders;
        end if;
      end Entropy_Calculations;

      procedure Entropy_Output is
        pos_countdown : Natural := group_size;
        sel_idx : Positive_32 := 1;
        cluster : Positive;
        symbol : Max_Alphabet;
      begin
        for mtf_idx in 1 .. mtf_last loop
          cluster := selector (sel_idx);
          symbol := mtf_data (mtf_idx);

          Put_Bits
            (Unsigned_32
              (descr (cluster) (symbol).code),
               descr (cluster) (symbol).bit_length);

          pos_countdown := pos_countdown - 1;
          if pos_countdown = 0 then
            pos_countdown := group_size;
            sel_idx := sel_idx + 1;
          end if;
        end loop;
      end Entropy_Output;

      procedure Put_Block_Header is
      begin
        Put_Bits (block_header_magic);
        block_crc := CRC.Final (block_crc);
        Put_Bits (block_crc, 32);
        Trace ("Block CRC:   " & block_crc'Image, detailed);
        combined_crc := Rotate_Left (combined_crc, 1) xor block_crc;
        Trace ("Combined CRC:" & combined_crc'Image, detailed);
        Put_Bits (0, 1);  --  Randomized flag, always False.
        Put_Bits (Unsigned_32 (bwt_index), 24);
      end Put_Block_Header;

      procedure Put_Block_Trees_Descriptors is

        procedure Put_Mapping_Table is
          in_use_16 : array (Byte range 0 .. 15) of Boolean := (others => False);
        begin
          for i in in_use_16'Range loop
            for j in in_use_16'Range loop
              if in_use (i * 16 + j) then
                in_use_16 (i) := True;
              end if;
            end loop;
          end loop;

          --  Send the first 16 bits which tell which pieces are stored.
          for i in in_use_16'Range loop
            Put_Bits (in_use_16 (i));
          end loop;
          --  Send detail of the used pieces.
          for i in in_use_16'Range loop
            if in_use_16 (i) then
              for j in in_use_16'Range loop
                Put_Bits (in_use (i * 16 + j));
              end loop;
            end if;
          end loop;
        end Put_Mapping_Table;

        procedure Put_Selectors is
          value : array (1 .. entropy_coder_count) of Positive;
          mtf_idx : Positive;
        begin
          Put_Bits (Unsigned_32 (selector_count), 15);
          for w in value'Range loop
            --  We start with 1, 2, 3, ...:
            value (w) := w;
          end loop;
          for i in 1 .. selector_count loop
            for search in value'Range loop
              if value (search) = selector (i) then
                mtf_idx := search;
                exit;
              end if;
            end loop;
            --  Move the value to the first place.
            for j in reverse 2 .. mtf_idx loop
              value (j) := value (j - 1);
            end loop;
            value (1) := selector (i);
            --  MTF-transformed index for the selected entropy coder.
            for bar in 1 .. mtf_idx  - 1 loop
              --  Output as many '1' bit as the value of mtf_idx - 1:
              Put_Bits (1, 1);
            end loop;
            Put_Bits (0, 1);
          end loop;
        end Put_Selectors;

        procedure Put_Huffman_Bit_Lengths is
          current_bit_length, new_bit_length : Natural;
        begin
          for coder in 1 .. entropy_coder_count loop
            current_bit_length := descr (coder)(0).bit_length;
            Put_Bits (Unsigned_32 (current_bit_length), 5);
            for i in 0 .. last_symbol_in_use loop
              new_bit_length := descr (coder)(i).bit_length;
              Adjust_Bit_length :
              loop
                if current_bit_length = new_bit_length then
                  Put_Bits (0, 1);
                  exit Adjust_Bit_length;
                else
                  Put_Bits (1, 1);
                  if current_bit_length < new_bit_length then
                    current_bit_length := current_bit_length + 1;
                    Put_Bits (0, 1);
                  else
                    current_bit_length := current_bit_length - 1;
                    Put_Bits (1, 1);
                  end if;
                end if;
              end loop Adjust_Bit_length;
            end loop;
          end loop;
        end Put_Huffman_Bit_Lengths;

      begin
        Put_Mapping_Table;
        Put_Bits (Unsigned_32 (entropy_coder_count), 3);
        Put_Selectors;
        Put_Huffman_Bit_Lengths;
      end Put_Block_Trees_Descriptors;

    begin
      block_counter := block_counter + 1;
      Trace ("Block" & block_counter'Image, headlines);
      --  Data acquisition and transformation:
      RLE_1;
      BWT;
      MTF_and_RLE_2;
      Entropy_Calculations;
      --  Now we output the block's compressed data:
      Put_Block_Header;
      Put_Block_Trees_Descriptors;
      Entropy_Output;
    end Encode_Block;

    procedure Write_Stream_Header is
      magic : String := stream_header_magic;
    begin
      magic (magic'Last) := Character'Val (Character'Pos ('0') + level);
      Put_Bits (magic);
    end Write_Stream_Header;

    procedure Write_Stream_Footer is
    begin
      Put_Bits (stream_footer_magic);
      Put_Bits (combined_crc, 32);
      if bit_pos < 7 then
        Flush_Bit_Buffer;
      end if;
    end Write_Stream_Footer;

  begin
    Write_Stream_Header;
    data := new Buffer (1 .. block_capacity);
    loop
      Encode_Block;
      exit when not More_Bytes;
    end loop;
    Unchecked_Free (data);
    Write_Stream_Footer;
  end Encode;

end BZip2.Encoding;
