--  BZip2.Encoding - a standalone, generic BZip2 encoder.
------------------
--
--  Examples of use:
--    BZip2_Enc, a standalone encoder to .bz2 files
--    Zip.Compress.BZip2_E, creates Zip files entries with BZip2 encoding

--  Legal licensing note:

--  Copyright (c) 2024 .. 2025 Gautier de Montmollin
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

--  To do:
--  -----
--
--    - BWT performance: consider a specialized sorting algorithm to solve the
--        slowness of compressing very redundant data.
--        For seeing the issue: try compressing a text with 50000 lines of the
--        same short text, and see what happens.
--    - BWT performance: consider Suffix-Array-Induced-Sorting.
--        See the Suffix_Arrays package in ./extras and
--            https://github.com/dsnet/compress/blob/master/bzip2/bwt.go
--            https://sites.google.com/site/yuta256/sais
--        Note that J. Seward kept a sorting algorithm.
--    - Segmentation: brute-force recursive binary segmentation as in EncodeBlock2 in
--        7-Zip's BZip2Encoder.cpp .
--    - Use tasking to parallelize the block compression jobs.
--
--  Already tried without significant success:
--  -----------------------------------------
--
--    - Use the permutation of entropy coders that minimizes the
--        size of compression structure.
--    - Brute-force over different strategies to tweak frequencies for avoiding
--        zero occurrences (see Avoid_Zeros). Unfortunately, the gains are offset by larger
--        compression structures (the Huffman trees descriptors take more room).
--    - Use k-means machine learning method to re-allocate clusters to entropy coders.
--        Removed from code on 2025-02-23.
--    - Use a "noisiness" (instead of a "bumpiness") function of a group's frequencies
--        in the sorting key for the initial clustering.
--    - Use a "mid-bumpiness" score, subtracted from the "left-bumpiness" (times a factor),
--        in the sorting key for the initial clustering.
--    - Initial clustering: try an uneven distribution of the number of members per
--        cluster. Formulas used: first cluster with half the number of strings of last
--        cluster, linear inbetween; or vice-versa.
--    - Set up the initial clustering by slicing the global frequency histogram
--        "horizontally" (on the symbol axis) to create artificial truncated histograms
--        and allocate them to the data groups. It obviously traps the model most of the time
--        into a suboptimal local optimum in the 258-dimensional criterion space.
--        This method is used by the original BZip2 program.
--        Removed from code on 2025-02-08.

with Data_Segmentation;
with Huffman.Encoding.Length_Limited_Coding;

with Ada.Containers.Generic_Constrained_Array_Sort,
     Ada.Strings.Unbounded,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

package body BZip2.Encoding is

  procedure Encode
    (option    : Compression_Option := block_900k;
     size_hint : Stream_Size_Type   := unknown_size)
  is
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
         when block_100k => 1,
         when block_400k => 4,
         when block_900k => 9);

    block_capacity : constant Natural_32 := sub_block_size * level;

    --  We use in this package 4 large heap-allocated arrays.
    --  It is possible to use Ada.Containers.Vectors but the run time
    --  is longer, possibly due to indirect access to data and various
    --  checks. For instance, replacing Buffer below with a Vector makes
    --  the encoding ~26% slower.

    type Buffer is array (Natural_32 range <>) of Byte;
    type Buffer_Access is access Buffer;

    procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Buffer, Buffer_Access);

    combined_crc : Unsigned_32 := 0;

    block_counter : Natural := 0;

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

    --  Each block is limited either by the data available
    --  by the block capacity.
    --  It means that each encoding step has an end and
    --  that we can theoretically go on with the next step,
    --  perhaps at the price of using more memory.

    procedure Encode_Block (raw_buf : Buffer) is

      -----------------------------------
      --  Initial Run-Length Encoding  --
      -----------------------------------

      rle_1_block_size : Natural_32 := 0;
      block_crc : Unsigned_32;
      in_use : array (Byte) of Boolean := (others => False);

      rle_1_data : Buffer_Access := new Buffer (1 .. block_capacity * 5 / 4);
      --  Worst case: all data consist of runs of 4 bytes -> 5 bytes with RLE_1.

      procedure RLE_1 is
        b_prev : Byte := 0;  --  Initialization is to reassure the compiler.
        run : Natural := 0;

        procedure Store_Run with Inline is
          procedure Store (x : Byte) with Inline is
          begin
            rle_1_block_size := rle_1_block_size + 1;
            rle_1_data (rle_1_block_size) := x;
            in_use (x) := True;
          end Store;
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
        for b of raw_buf loop
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
        Trace ("RLE_1: raw buffer length:  " & raw_buf'Length'Image,   headlines);
        Trace ("RLE_1-processed block size:" & rle_1_block_size'Image, headlines);
        if verbosity_level >= super_detailed then
          Trace ("RLE_1: ", rle_1_data (1 .. rle_1_block_size), super_detailed);
        end if;
      end RLE_1;

      ---------------------------------
      --  Burrows-Wheeler Transform  --
      ---------------------------------

      bwt_data : Buffer_Access;
      bwt_index : Natural_32 := 0;  --  0-based.

      procedure BWT is

        subtype Offset_Range is Integer_32 range 0 .. rle_1_block_size - 1;

        type Offset_Table is array (Offset_Range) of Offset_Range;
        type Offset_Table_Access is access Offset_Table;

        procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Offset_Table, Offset_Table_Access);

        --  Compare the message, rotated with two different offsets.
        function Lexicographically_Smaller (left, right : Offset_Range) return Boolean with Inline is
          il, ir : Integer_32;
          l, r : Byte;
        begin
          pragma Assert (rle_1_data'First = 1);
          il := 1 + (if left  = 0 then 0 else rle_1_block_size - left);
          ir := 1 + (if right = 0 then 0 else rle_1_block_size - right);
          for i in Offset_Range loop
            l := rle_1_data (il);
            r := rle_1_data (ir);
            if l < r then
              return True;
            elsif l > r then
              return False;
            end if;
            il := il + 1;
            if il > rle_1_block_size then
              il := 1;
            end if;
            ir := ir + 1;
            if ir > rle_1_block_size then
              ir := 1;
            end if;
          end loop;
          --  Equality in contents.
          return left < right;  --  Ensures stable sorting.
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

        bwt_data := new Buffer (1 .. rle_1_block_size);
        for i in Offset_Range loop
          --  Copy last column of the matrix into transformed message:
          bwt_data (1 + i) := rle_1_data (1 + (rle_1_block_size - 1 - offset (i)) mod rle_1_block_size);
          if offset (i) = 0 then
            --  Found the row index of the original message.
            bwt_index := i;
          end if;
        end loop;

        if verbosity_level >= super_detailed then
          if rle_1_block_size = 0 then
            Trace ("BWT:   (empty block)", super_detailed);
          else
            Trace ("BWT:   ", bwt_data.all, super_detailed);
            Trace ("BWT index:" & bwt_index'Image, super_detailed);
          end if;
        end if;
        Unchecked_Free (offset);
        Unchecked_Free (rle_1_data);
      exception
        when others =>
          Unchecked_Free (offset);
          raise;
      end BWT;

      ----------------------------------------------------
      --  Move-to-Front and second Run-Length Encoding  --
      ----------------------------------------------------

      subtype Max_Alphabet is Integer range 0 .. max_alphabet_size - 1;

      type MTF_Array is array (Positive_32 range <>) of Max_Alphabet;
      type MTF_Array_Access is access MTF_Array;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (MTF_Array, MTF_Array_Access);

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
          --  ^ + 3 : the special symbols RUN_A, RUN_B, EOB
          --    - 1 : value 0 has no symbol (RUN_A and RUN_B are used for the runs of 0)
          --    - 1 : zero-based

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
          rc : Unsigned_32;
        begin
          if run > 0 then
            --  Output a binary representation of `run`
            --  using RUN_A for 0's RUN_B for 1's.
            rc := Unsigned_32 (run + 1);
            loop
              Store (Max_Alphabet (rc and 1));
              rc := Shift_Right (rc, 1);
              exit when rc < 2;
            end loop;
            --  Reset the run count.
            run := 0;
          end if;
        end Store_Run;

        mtf_symbol : array (0 .. 255) of Byte;
        idx : Natural;
        bt_seq : Byte;

      begin
        Prepare_Mapping;

        mtf_data := new MTF_Array (1 .. 1 + 2 * rle_1_block_size);

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
            Store (1 + idx);  --  Value stored is >= 2. Values 0 and 1 are RUN_A, RUN_B.
          end if;

        end loop Big_MTF_RLE_2_Loop;

        Store_Run;
        Store (EOB);

        Unchecked_Free (bwt_data);
      end MTF_and_RLE_2;

      ----------------------
      --  Entropy Coding  --
      ----------------------

      type Entropy_Coder_Range is range 1 .. max_entropy_coders;

      descr :
        array (Entropy_Coder_Range) of
          Huffman.Encoding.Descriptor (Max_Alphabet);

      entropy_coder_count : Entropy_Coder_Range
        range 2 .. Entropy_Coder_Range'Last;
      --      ^ count = 1 is rejected by the canonical BZip2 decoder.

      selector_count : Integer_32;

      selector : array (1 .. 1 + block_capacity / group_size) of Entropy_Coder_Range;

      procedure Entropy_Calculations is

        subtype Alphabet_in_Use is Integer range 0 .. last_symbol_in_use;

        type Count_Array is array (Alphabet_in_Use) of Natural_32;

        procedure Avoid_Zeros (freq : in out Count_Array) is
          zeroes : Natural := 0;
        begin
          for stat of freq loop
            if stat = 0 then
              zeroes := zeroes + 1;
            end if;
          end loop;
          case zeroes is
            when 0 =>
              --  Zero zeroes, zero problem :-)
              null;
            when 1 .. 100 =>
              --  Turn the "0"'s into "1"'s.
              for stat of freq loop
                stat := Natural_32'Max (1, stat);
              end loop;
            when others =>
              --  Turn the "0"'s into an actual "1/2".
              for stat of freq loop
                stat := (if stat = 0 then 1 else stat * 2);
              end loop;
          end case;
        end Avoid_Zeros;

        max_code_len : Positive;

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

        type Huffman_Length_Array is array (Alphabet_in_Use) of Natural;

        procedure Define_Descriptor (freq : in out Count_Array; des : Entropy_Coder_Range) is
          procedure LLHCL is new
            Huffman.Encoding.Length_Limited_Coding
              (Alphabet     => Alphabet_in_Use,
               Count_Type   => Natural_32,
               Count_Array  => Count_Array,
               Length_Array => Huffman_Length_Array,
               max_bits     => max_code_len);
          len : Huffman_Length_Array;
          pragma Assert (max_code_len <= max_code_len_bzip2_1_0_2);
        begin
          Avoid_Zeros (freq);
          LLHCL (freq, len);
          for symbol in Alphabet_in_Use loop
            descr (des)(symbol).bit_length := len (symbol);
          end loop;
          Huffman.Encoding.Prepare_Codes
            (descr (des)(Alphabet_in_Use), max_code_len, False);
        end Define_Descriptor;

        -----------------------------------------------------
        --  Radically simple but functional entropy coder  --
        -----------------------------------------------------

        procedure Single_Entropy_Coder is
          freq : Count_Array := (others => 0);
        begin
          for symbol of mtf_data (1 .. mtf_last) loop
            freq (symbol) := freq (symbol) + 1;
          end loop;
          max_code_len := max_code_len_bzip2_1_0_3;
          Define_Descriptor (freq, 1);
          entropy_coder_count := 2;  --  The canonical BZip2 decoder requires >= 2 coders.
          descr (2) := descr (1);    --  We actually don't use the copy (psssht), but need to define it!
          for i in 1 .. selector_count loop
            selector (Integer_32 (i)) := 1;
          end loop;
        end Single_Entropy_Coder;

        --------------------------------------------------------------------------
        --  Define multiple entropy coders (max 6) and assign them to the       --
        --  various groups of data (max 18000, with 50 symbols in each group).  --
        --  The art is to gather the groups into meaningful clusters.           --
        --  Each cluster will use one of the entropy coders.                    --
        --------------------------------------------------------------------------

        procedure Multiple_Entropy_Coders is

          subtype Selector_Range is Positive_32 range 1 .. selector_count;

          --  Create an initial clustering depending on a ranking using a key
          --  computed on a subset of the alphabet (a mix of RUN_A, RUN_B,
          --  low-index MTF values).
          --  Look at the first few columns of some output
          --  of Output_Frequency_Matrix to find why.
          --
          procedure Initial_Clustering_Ranking_Method (sample_width : Positive) is

            type Pair is record
              key   : Natural_32;   --  Occurrences of symbols that are in the subset.
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

            type Cluster_Attribution is array (Positive range <>) of Entropy_Coder_Range;

            procedure Initial_Clustering_by_Rank (attr : Cluster_Attribution) is
              na : constant Positive_32 := attr'Length;
              ns : constant Selector_Range := selector_count;
              a32, low, high : Natural_32;
            begin
              low := 1;
              for attr_idx in attr'Range loop
                a32 := Integer_32 (attr_idx) - Integer_32 (attr'First) + 1;  --  a32 = 1, 2, 3, .. na.
                high := a32 * ns / na;
                for i in low .. high loop
                  --  When ns < na, it will happen that this loop's range
                  --  is empty for some values of a32. It is expected.
                  selector (ranking (i).index) := attr (attr_idx);
                end loop;
                low := 1 + high;
              end loop;
            end Initial_Clustering_by_Rank;

            pos_countdown : Natural := group_size;
            sel_idx : Positive_32 := 1;
            key : Natural_32 := 0;
            last_symbol_sampled : constant Natural := Integer'Min (EOB - 1, run_a + sample_width - 1);
            symbol : Alphabet_in_Use;
          begin
            --  Populate the frequency stats for the ranking, grouped by data group.
            --
            for mtf_idx in 1 .. mtf_last loop
              symbol := mtf_data (mtf_idx);
              if symbol in run_a .. last_symbol_sampled then
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

            --  The construction of initial clusters can now be
            --  done easily using the following sorting:
            --
            Ranking_Sort (ranking);

            --  Example with two clusters:
            --   - Low values (more random data) for the cluster #2.
            --   - High values (more redundant data) for #1.
            --
            case entropy_coder_count is
              when 2 => Initial_Clustering_by_Rank ((2, 1));
              when 3 => Initial_Clustering_by_Rank ((3, 1, 2));
              when 4 => Initial_Clustering_by_Rank ((4, 2, 1, 3));
              when 5 => Initial_Clustering_by_Rank ((5, 3, 1, 2, 4));
              when 6 => Initial_Clustering_by_Rank ((6, 4, 2, 1, 3, 5));
            end case;

          end Initial_Clustering_Ranking_Method;

          procedure Define_Descriptors is
            pos_countdown : Natural             := group_size;
            selector_idx  : Positive_32         := 1;
            cluster       : Entropy_Coder_Range := selector (1);
            symbol        : Alphabet_in_Use;
            freq_cluster : array (1 .. entropy_coder_count) of Count_Array :=  (others => (others => 0));
          begin
            --  Populate the frequency stats, grouped by cluster (= entropy coder choice):
            for mtf_idx in 1 .. mtf_last loop
              symbol := mtf_data (mtf_idx);
              freq_cluster (cluster)(symbol) := freq_cluster (cluster)(symbol) + 1;
              pos_countdown := pos_countdown - 1;
              if pos_countdown = 0 and then mtf_idx < mtf_last then
                pos_countdown := group_size;
                selector_idx := selector_idx + 1;
                cluster := selector (selector_idx);
              end if;
            end loop;
            --  Create Huffman codes based on the said frequencies:
            for cl in 1 .. entropy_coder_count loop
              Define_Descriptor (freq_cluster (cl), cl);
            end loop;
          end Define_Descriptors;

          defector_groups : Natural;

          procedure Simulate_Entropy_Coding_Variants_and_Reclassify is
            pos_countdown : Natural             := group_size;
            selector_idx  : Positive_32         := 1;
            cluster       : Entropy_Coder_Range := selector (1);
            symbol        : Alphabet_in_Use;
            bit_count     : array (1 .. entropy_coder_count) of Natural := (others => 0);

            --  We simulate the encoding of selectors (for its cost).
            mtf_cluster_value : array (1 .. entropy_coder_count) of Entropy_Coder_Range;
            mtf_cluster_index : Entropy_Coder_Range;

            procedure Optimize_Group is
              min_bits : Natural := Natural'Last;
              best : Entropy_Coder_Range := cluster;
              cost : Natural;
            begin
              --  At this point we have computed the costs in bits for
              --  encoding the current group of data using various entropy coders.
              --  Now we look at the extra cost of switching entropy coders.
              for cl in 1 .. entropy_coder_count loop
                cost := bit_count (cl);
                --  Here we account the mtf encoding of the selectors.
                for search in mtf_cluster_value'Range loop
                  if mtf_cluster_value (search) = cl then
                    mtf_cluster_index := search;
                    exit;
                  end if;
                end loop;
                cost := cost + Natural (mtf_cluster_index);
                --
                if cost < min_bits then
                  --  Encoder #cl is cheaper.
                  min_bits := cost;
                  best := cl;
                end if;
              end loop;

              if best /= cluster then
                --  We have found a cheaper encoding by switching to another cluster.
                --  -> the group #sel_idx changes party (re-allocation).
                selector (selector_idx) := best;
                defector_groups := defector_groups + 1;
              end if;

              --  Now do the "definitive" (but still simulated)
              --  mtf for the chosen cluster index.
              for search in mtf_cluster_value'Range loop
                if mtf_cluster_value (search) = selector (selector_idx) then
                  mtf_cluster_index := search;
                  exit;
                end if;
              end loop;
              --  Move the value to the first place.
              for j in reverse 2 .. mtf_cluster_index loop
                mtf_cluster_value (j) := mtf_cluster_value (j - 1);
              end loop;
              mtf_cluster_value (1) := selector (selector_idx);
            end Optimize_Group;

          begin
            --  Cost analysis by simulation and re-classification
            --  (or said otherwise, re-allocation).
            --
            for w in mtf_cluster_value'Range loop
              --  We start with 1, 2, 3, ...:
              mtf_cluster_value (w) := w;
            end loop;

            defector_groups := 0;
            pos_countdown := group_size;
            for mtf_idx in 1 .. mtf_last loop
              symbol := mtf_data (mtf_idx);
              for cl in 1 .. entropy_coder_count loop
                 --  For each cluster cl, simulate output assuming
                 --  the current group belongs to cluster cl.
                 bit_count (cl) := bit_count (cl) + descr (cl) (symbol).bit_length;
              end loop;
              pos_countdown := pos_countdown - 1;
              if pos_countdown = 0 then
                Optimize_Group;
                pos_countdown := group_size;
                if mtf_idx < mtf_last then
                  bit_count := (others => 0);
                  selector_idx := selector_idx + 1;
                  cluster := selector (selector_idx);
                end if;
              end if;
            end loop;
            if pos_countdown < group_size then
              --  Optimize last, incomplete group.
              Optimize_Group;
            end if;
          end Simulate_Entropy_Coding_Variants_and_Reclassify;

          low_cluster_usage : Boolean := False;

          procedure Cluster_Statistics is
            stat_cluster : array (Entropy_Coder_Range) of Natural_32 := (others => 0);
            cl : Entropy_Coder_Range;
            uniform_usage : constant Natural_32 := selector_count / Natural_32 (entropy_coder_count);
            threshold_denominator : constant := 2;
          begin
            low_cluster_usage := False;
            --  Compute cluster usage.
            for i in Selector_Range loop
              cl := selector (i);
              stat_cluster (cl) := stat_cluster (cl) + 1;
            end loop;
            for c in 1 .. entropy_coder_count loop
              Trace
                 ("          Cluster" & c'Image & " is used by" &
                  stat_cluster (c)'Image & " groups.", detailed);
              if stat_cluster (c) < uniform_usage / threshold_denominator then
                low_cluster_usage := True;
                Trace ("          ---> Low Cluster Usage!", detailed);
              end if;
            end loop;
          end Cluster_Statistics;

          procedure Construct (sample_width : Natural) is
            reclassification_iteration_limit : constant := 10;
          begin
            Initial_Clustering_Ranking_Method (sample_width);
            Trace
              ("   Construct with" & entropy_coder_count'Image & " coders", detailed);

            --  Compute the entropy coders based on the current
            --  clustering, then refine the (group -> cluster) attribution.
            --  A group can join another cluster if the number of bits
            --  in the output is smaller. However, it will influence the
            --  frequencies of both affected clusters.
            --
            for iteration in 1 .. reclassification_iteration_limit loop
              Cluster_Statistics;
              Define_Descriptors;
              Simulate_Entropy_Coding_Variants_and_Reclassify;
              Trace
                ("   Iteration" & iteration'Image &
                 ". Defector groups:" & defector_groups'Image,
                 detailed);
              exit when defector_groups = 0;
            end loop;
            if defector_groups > 0 then
              --  The cluster optimization loop has exited before
              --  full stabilization (clusters have changed).
              Define_Descriptors;
            end if;
            Cluster_Statistics;
          end Construct;

          function Compute_Total_Entropy_Cost return Natural_32 is
            --  We simulate the sending of the whole block and
            --  look at the costs related to the entropy coding.

            function Compute_Selectors_Cost return Natural_32 is
              mtf_cluster_value : array (1 .. entropy_coder_count) of Entropy_Coder_Range;
              mtf_cluster_index : Entropy_Coder_Range;
              bits : Natural_32 := 0;
            begin
              for w in mtf_cluster_value'Range loop
                mtf_cluster_value (w) := w;
              end loop;
              for i in Selector_Range loop
                for search in mtf_cluster_value'Range loop
                  if mtf_cluster_value (search) = selector (i) then
                    mtf_cluster_index := search;
                    exit;
                  end if;
                end loop;
                for j in reverse 2 .. mtf_cluster_index loop
                  mtf_cluster_value (j) := mtf_cluster_value (j - 1);
                end loop;
                mtf_cluster_value (1) := selector (i);
                bits := bits + Integer_32 (mtf_cluster_index);
              end loop;
              return bits;
            end Compute_Selectors_Cost;

            function Compute_Huffman_Bit_Lengths_Cost return Natural_32 is
              current_bit_length, new_bit_length : Natural;
              bits : Natural_32 := 0;
            begin
              for coder in 1 .. entropy_coder_count loop
                current_bit_length := descr (coder)(0).bit_length;
                bits := bits +  5;
                for i in 0 .. last_symbol_in_use loop
                  new_bit_length := descr (coder)(i).bit_length;
                  Adjust_Bit_length :
                  loop
                    if current_bit_length = new_bit_length then
                      bits := bits + 1;
                      exit Adjust_Bit_length;
                    else
                      bits := bits + 2;
                      if current_bit_length < new_bit_length then
                        current_bit_length := current_bit_length + 1;
                      else
                        current_bit_length := current_bit_length - 1;
                      end if;
                    end if;
                  end loop Adjust_Bit_length;
                end loop;
              end loop;
              return bits;
            end Compute_Huffman_Bit_Lengths_Cost;

            pos_countdown : Natural             := group_size;
            selector_idx  : Positive_32         := 1;
            cluster       : Entropy_Coder_Range := selector (1);
            bits          : Natural_32 := 0;
          begin
            --  Simulate the sending of the data itself:
            for mtf_idx in 1 .. mtf_last loop
              bits := bits + Natural_32 (descr (cluster)(mtf_data (mtf_idx)).bit_length);
              pos_countdown := pos_countdown - 1;
              if pos_countdown = 0 and then mtf_idx < mtf_last then
                pos_countdown := group_size;
                selector_idx := selector_idx + 1;
                cluster := selector (selector_idx);
              end if;
            end loop;
            --  We add to the compressed data cost, the cost of switching coders
            --  over the whole block and the cost of sending the compression
            --  structures of the coders.
            bits := bits + Compute_Selectors_Cost + Compute_Huffman_Bit_Lengths_Cost;
            return bits;
          end Compute_Total_Entropy_Cost;

          cost              : Natural_32;
          best_cost         : Natural_32 := Natural_32'Last;
          best_ec_count     : Entropy_Coder_Range;
          best_max_code_len : Positive;
          best_sample_width : Natural;

          type Value_Array is array (Positive range <>) of Natural;

          -----------------------------------------------------------------
          --  Choices for brute-force search of the best entropy coding  --
          -----------------------------------------------------------------

          max_code_len_choices : constant Value_Array :=
          (case option is
             when block_100k => (1 => 16),
             when block_400k => (1 => 16),
             when block_900k => (15, 17));

          coder_choices : constant Value_Array :=
          (case option is
             when block_100k => (4, 6),
             when block_400k => (4, 6),
             when block_900k =>
               (case mtf_last is
                  when     1 ..  5_000 => (2, 3, 6),
                  when 5_001 .. 10_000 => (3, 4, 6),
                  when others          => (3, 4, 5, 6)));

          sample_width_choices : constant Value_Array :=
          (case option is
             when block_100k => (1 => 4),
             when block_400k => (1 => 4),
             when block_900k => (3, 4));

          --  In a former version, we had 210 combinations of
          --  brute-force choices for option block_900k, making
          --  that option run 13x longer that with only 1 combination!
          --  See also timings in doc/za_work.xls, sheet BZip2.

        begin
          --  Brute-force: test some max code lengths:
          for max_code_len_test of max_code_len_choices loop
            max_code_len := max_code_len_test;
            --  Brute-force: test some sample widths:
            for sample_width_test of sample_width_choices loop
              --  Brute-force: test some amounts of entropy coders:
              for ec_test in reverse Entropy_Coder_Range range min_entropy_coders .. max_entropy_coders loop
                if low_cluster_usage
                  --  ^ At least one cluster of previous iteration is not used much.
                  or else (for some value of coder_choices => Entropy_Coder_Range (value) = ec_test)
                then
                  entropy_coder_count := ec_test;
                  Construct (sample_width_test);
                  cost := Compute_Total_Entropy_Cost;
                  if cost < best_cost then
                    best_cost         := cost;
                    best_ec_count     := ec_test;
                    best_max_code_len := max_code_len;
                    best_sample_width := sample_width_test;
                  end if;
                end if;
              end loop;
            end loop;
          end loop;

          max_code_len        := best_max_code_len;
          entropy_coder_count := best_ec_count;
          Trace
            ("Max len:" & max_code_len'Image &
             ", coders:" & entropy_coder_count'Image &
             ", sample width:" & best_sample_width'Image,
             detailed);
          Construct (best_sample_width);
        end Multiple_Entropy_Coders;

        trace_frequency_matrix : constant Boolean := False;
        use_single_coder : constant Boolean := False;

      begin
        selector_count := 1 + (mtf_last - 1) / group_size;
        if trace_frequency_matrix then
          Output_Frequency_Matrix;
        end if;

        if use_single_coder then
          Single_Entropy_Coder;
        else
          Multiple_Entropy_Coders;
        end if;
      end Entropy_Calculations;

      ---------------------------------
      --  Output of compressed data  --
      ---------------------------------

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
          mtf_cluster_value : array (1 .. entropy_coder_count) of Entropy_Coder_Range;
          mtf_cluster_index : Entropy_Coder_Range;
        begin
          Put_Bits (Unsigned_32 (selector_count), 15);
          for w in mtf_cluster_value'Range loop
            --  We start with 1, 2, 3, ...:
            mtf_cluster_value (w) := w;
          end loop;
          for i in 1 .. selector_count loop
            for search in mtf_cluster_value'Range loop
              if mtf_cluster_value (search) = selector (i) then
                mtf_cluster_index := search;
                exit;
              end if;
            end loop;
            --  Move the value to the first place.
            for j in reverse 2 .. mtf_cluster_index loop
              mtf_cluster_value (j) := mtf_cluster_value (j - 1);
            end loop;
            mtf_cluster_value (1) := selector (i);
            --  MTF-transformed index for the selected entropy coder.
            for bar in 1 .. mtf_cluster_index  - 1 loop
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

      procedure Entropy_Output is
        pos_countdown : Natural             := group_size;
        selector_idx  : Positive_32         := 1;
        cluster       : Entropy_Coder_Range := selector (1);
        symbol        : Max_Alphabet;
      begin
        for mtf_idx in 1 .. mtf_last loop
          symbol := mtf_data (mtf_idx);

          Put_Bits
            (Unsigned_32
              (descr (cluster) (symbol).code),
               descr (cluster) (symbol).bit_length);

          pos_countdown := pos_countdown - 1;
          if pos_countdown = 0 and then mtf_idx < mtf_last then
            pos_countdown := group_size;
            selector_idx := selector_idx + 1;
            cluster := selector (selector_idx);
          end if;
        end loop;

        Unchecked_Free (mtf_data);
      end Entropy_Output;

    begin
      block_counter := block_counter + 1;
      Trace ("Block" & block_counter'Image, headlines);

      --  Data transformation (no output):
      RLE_1;
      BWT;
      MTF_and_RLE_2;
      Entropy_Calculations;

      --  Now we output the block's compressed data:
      Put_Block_Header;
      Put_Block_Trees_Descriptors;
      Entropy_Output;

    exception
      when others =>
        Unchecked_Free (rle_1_data);
        Unchecked_Free (bwt_data);
        Unchecked_Free (mtf_data);
        raise;
    end Encode_Block;

    stream_rest : Stream_Size_Type := size_hint;

    --------------------------------------------
    --  Data acquisition and block splitting  --
    --------------------------------------------

    procedure Read_and_Split_Block (dyn_block_capacity : Natural_32) is

      --  In the cases RLE_1 compression is efficient, the
      --  input buffer can contain much more that the post RLE_1 block.
      --  Best case: all runs of 259 bytes, factor 259/5 = 51.8.
      --  The latter has to fit into the agreed capacity (a multiple of 100_000).
      --  So, we define a conveniently large input buffer.

      multiplier : constant := 10;

      raw_buf : Buffer_Access := new Buffer (1 .. multiplier * dyn_block_capacity);

      package Segmentation_for_BZip2 is
        new Data_Segmentation
          (Index                 => Natural_32,
           Alphabet              => Byte,
           Buffer_Type           => Buffer,
           discrepancy_threshold => 2.0,
           index_threshold       => 80_000,
           window_size           => 80_000);

      single_segment : constant Boolean := False;
      seg : Segmentation_for_BZip2.Segmentation;

      raw_buf_index : Natural_32 := 0;
      index_start   : Natural_32 := 1;

      --  We have to simulate RLE_1 to avoid block size overflows
      --  in the decoder.
      --  RLE_1 often expands the data (and sometimes does it
      --  considerably) when it meets runs of length 4: 5 bytes are
      --  stored in that case.
      --  So the worst case expansion is by a factor 5/4.

      rle_1_block_size : Natural_32 := 0;
      b : Byte;
      b_prev : Byte := 0;  --  Initialization is to reassure the compiler.
      run : Natural := 0;

      procedure Simulate_Store_Run with Inline is
      begin
        rle_1_block_size := rle_1_block_size + Integer_32 (Integer'Min (4, run));
        if run >= 4 then
          pragma Assert (run <= 259);
          rle_1_block_size := rle_1_block_size + 1;
        end if;
        run := 1;
      end Simulate_Store_Run;

      start : Boolean := True;

    begin
      --  Data acquisition:
      while More_Bytes
        and then rle_1_block_size + 5 < dyn_block_capacity
        --  ^ The +5 is because sometimes a pack of max 5 bytes is sent by Store_Run.
        and then raw_buf_index < raw_buf'Last
      loop
        b := Read_Byte;
        raw_buf_index := raw_buf_index + 1;
        raw_buf (raw_buf_index) := b;
        if stream_rest /= unknown_size then
          stream_rest := stream_rest - 1;
        end if;
        if start or else b /= b_prev then
          --  Startup or Run break:
          Simulate_Store_Run;
          start := False;
        elsif run = 259 then
          --  Force a run break, even though b = b_prev:
          Simulate_Store_Run;
        else
          run := run + 1;
        end if;
        b_prev := b;
      end loop;
      Simulate_Store_Run;

      if single_segment then
        --  No segmentation /splitting:
        Encode_Block (raw_buf (1 .. raw_buf_index));
      else
        Segmentation_for_BZip2.Segment_by_Entropy (raw_buf (1 .. raw_buf_index), seg);

        if seg.Is_Empty then
          Encode_Block (raw_buf (1 .. 0));
        else
          for s of seg loop
            Encode_Block (raw_buf (index_start .. s));
            index_start := s + 1;
          end loop;
        end if;

        if Integer (seg.Length) > 1 then
          Trace ("Segmentation into" & seg.Length'Image & " segments", headlines);
          for s of seg loop
            Trace ("  Segment limit at" & s'Image, headlines);
          end loop;
        end if;

      end if;

      Unchecked_Free (raw_buf);
    exception
      when others =>
        Unchecked_Free (raw_buf);
        raise;
    end Read_and_Split_Block;

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

    --  Vertically challenged blocks.
    small_block_prop_min : constant := 0.05;  --  Below that, not worth the trouble.
    small_block_prop_max : constant := 0.30;  --  Above that, not worth the trouble either.

  begin
    Write_Stream_Header;
    loop
      if Float (stream_rest) in
        Float (block_capacity) * (1.0 + small_block_prop_min) ..
        Float (block_capacity) * (1.0 + small_block_prop_max)
      then
        --  Avoid encoding the last block as a "too" small one (poorer compression)
        --  if we can balance the last two blocks.
        --  NB: a more sophisticated balancing using (1.0 - small_block_prop_max)
        --  did not deliver convincing results.
        Read_and_Split_Block (Natural_32 (stream_rest) / 2);
      else
        Read_and_Split_Block (block_capacity);
      end if;
      exit when not More_Bytes;
    end loop;
    Write_Stream_Footer;
  end Encode;

end BZip2.Encoding;
