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
         when block_100k => 1,
         when block_400k => 4,
         when block_900k => 9);

    block_capacity : constant Natural_32 := sub_block_size * level;

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

        --  Compare the message, rotated with two different offsets.
        function Lexicographically_Smaller (left, right : Offset_Range) return Boolean with Inline is
          il, ir : Integer_32;
          l, r : Byte;
        begin
          pragma Assert (data'First = 1);
          il := 1 + ((-left)  mod block_size);
          ir := 1 + ((-right) mod block_size);
          for i in Offset_Range loop
            l := data (il);
            r := data (ir);
            if l < r then
              return True;
            elsif l > r then
              return False;
            end if;
            il := il + 1;
            if il > block_size then
              il := 1;
            end if;
            ir := ir + 1;
            if ir > block_size then
              ir := 1;
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
            Trace ("BWT:   (empty block)", super_detailed);
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

        mtf_data := new MTF_Array (1 .. 1 + 2 * block_size);
        --  ^ Check real worst-case capacity !!

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

      ----------------------
      --  Entropy Coding  --
      ----------------------

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
          --  factor : constant := 2;
        begin
          for a in Alphabet_in_Use loop
            if freq (a) = 0 then
              --  Tweak the stats to avoid zeros
              --  (the canonical BZip2 wants that)...
              for aa in Alphabet_in_Use loop
                --  Inrease each count by 1 to avoid 0 lengths
                freq (aa) := freq (aa) + 1;

                --  Alternative idea: turn the "0"'s into actual "1/factor".
                --  Curiously, the compression is worse despite a tweak
                --  that is closer to the actual proportions.
                --
                --  if freq (aa) = 0 then
                --    freq (aa) := 1;
                --  else
                --    freq (aa) := freq (aa) * factor;
                --  end if;
              end loop;
              return;
            end if;
          end loop;
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

          --  Frequencies, grouped by cluster (i.e. by selected entropy coder).
          freq_cluster : array (Entropy_Coder_Range) of Count_Array;

          defectors : Natural;

          procedure Define_Simulate_and_Reclassify is
            pos_countdown : Natural := group_size;
            sel_idx : Positive_32 := 1;
            symbol : Alphabet_in_Use;
            cluster : Entropy_Coder_Range;
            bits : array (1 .. entropy_coder_count) of Natural := (others => 0);

            --  We simulate the encoding of selectors (for its cost),
            mtf_cluster_value : array (1 .. entropy_coder_count) of Positive;
            mtf_cluster_idx : Positive;

            procedure Optimize_Group is
              min_bits : Natural := Natural'Last;
              best : Entropy_Coder_Range := cluster;
              cost : Natural;
            begin
              --  At this point we have computed the costs in bits for
              --  the whole group of data.
              for cl in 1 .. entropy_coder_count loop
                cost := bits (cl);
                --  Here we account the mtf encoding of the selectors.
                for search in mtf_cluster_value'Range loop
                  if mtf_cluster_value (search) = cl then
                    mtf_cluster_idx := search;
                    exit;
                  end if;
                end loop;
                cost := cost + mtf_cluster_idx;
                --
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

              --  Now do the "definitive" (but still simulated)
              --  mtf for the chosen cluster index.
              for search in mtf_cluster_value'Range loop
                if mtf_cluster_value (search) = selector (sel_idx) then
                  mtf_cluster_idx := search;
                  exit;
                end if;
              end loop;
              --  Move the value to the first place.
              for j in reverse 2 .. mtf_cluster_idx loop
                mtf_cluster_value (j) := mtf_cluster_value (j - 1);
              end loop;
              mtf_cluster_value (1) := selector (sel_idx);
            end Optimize_Group;

          begin
            --  Populate the frequency stats, grouped by cluster (entropy coder choice).
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

            --  Create Huffman codes based on the said frequencies.
            --
            for cl in 1 .. entropy_coder_count loop
              Define_Descriptor (freq_cluster (cl), cl);
            end loop;

            --  Cost analysis by simulation and re-classification
            --
            for w in mtf_cluster_value'Range loop
              --  We start with 1, 2, 3, ...:
              mtf_cluster_value (w) := w;
            end loop;

            defectors := 0;
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

          --  Create an initial clustering depending on a subset
          --  of the alphabet (RUN_A, RUN_B, low-index MTF values).
          --  Look at the first few columns of Output_Frequency_Matrix to find why.
          --
          procedure Initial_Clustering_Statistical_Method is

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
              ns : constant Positive_32 := selector_count;
              a32 : Positive_32;
            begin
              for attr_idx in attr'Range loop
                a32 := Integer_32 (attr_idx) - Integer_32 (attr'First) + 1;
                for i in 1 + (a32 - 1) * ns / na .. a32 * ns / na loop
                  selector (ranking (i).index) := attr (attr_idx);
                end loop;
              end loop;
            end Initial_Clustering_by_Rank;

            pos_countdown : Natural := group_size;
            sel_idx : Positive_32 := 1;
            key : Natural_32 := 0;
            symbol : Alphabet_in_Use;
          begin
            --  Populate the frequency stats for the ranking, grouped by data group.
            --
            for mtf_idx in 1 .. mtf_last loop
              symbol := mtf_data (mtf_idx);
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

            --  The construction of initial clusters can now be
            --  done easily using the following sorting:
            --
            Ranking_Sort (ranking);

            --  Create an initial clustering depending on a mix
            --  of RUN_A, RUN_B, low-index MTF occurrences.
            --  Example with two clusters:
            --   - Low values (more random data) for the cluster #2.
            --   - High values (more redundant data) for #1.
            --
            case entropy_coder_count is
              when 1 => null;  --  Not supported by canonical BZip2.
              when 2 => Initial_Clustering_by_Rank ((2, 1));
              when 3 => Initial_Clustering_by_Rank ((3, 1, 2));
              when 4 => Initial_Clustering_by_Rank ((4, 2, 1, 3));
              when 5 => Initial_Clustering_by_Rank ((5, 3, 1, 2, 4));
              when 6 => Initial_Clustering_by_Rank ((6, 4, 2, 1, 3, 5));
            end case;

          end Initial_Clustering_Statistical_Method;

          type Cluster_Statistics is array (Entropy_Coder_Range) of Natural;

          procedure Compute (stat_cluster : out Cluster_Statistics) is
            cl : Entropy_Coder_Range;
          begin
            stat_cluster := (others => 0);
            for i in 1 .. selector_count loop
              cl := selector (i);
              stat_cluster (cl) := stat_cluster (cl) + 1;
            end loop;
          end Compute;

          procedure Show_Cluster_Statistics with Inline is
            stat_cluster : Cluster_Statistics;
          begin
            if verbosity_level >= detailed then
              Compute (stat_cluster);
              for c in 1 .. entropy_coder_count loop
                Trace
                  ("          Cluster" & c'Image & " is used by" &
                   stat_cluster (c)'Image & " groups.", detailed);
              end loop;
            end if;
          end Show_Cluster_Statistics;

          procedure Construct is
            reclassification_iteration_limit : constant := 10;
          begin
            Initial_Clustering_Statistical_Method;
            Trace ("   Coders:" & entropy_coder_count'Image, detailed);

            --  Compute the entropy coders based on the initial
            --  clustering, then refine the (group -> cluster) attribution.
            --  A group can join another cluster if the number of bits
            --  in output is smaller. However, it will influence the
            --  frequencies of both affected clusters.
            --
            for iteration in 1 .. reclassification_iteration_limit loop
              Show_Cluster_Statistics;
              Define_Simulate_and_Reclassify;
              Trace
                ("   Iteration" & iteration'Image &
                 ". Defector groups:" & defectors'Image,
                 detailed);
              exit when defectors = 0;
            end loop;
            Show_Cluster_Statistics;
          end Construct;

          function Compute_Selectors_Cost return Natural_32 is
            value : array (1 .. entropy_coder_count) of Positive;
            mtf_idx : Positive;
            bits : Natural_32 := 0;
          begin
            for w in value'Range loop
              value (w) := w;
            end loop;
            for i in 1 .. selector_count loop
              for search in value'Range loop
                if value (search) = selector (i) then
                  mtf_idx := search;
                  exit;
                end if;
              end loop;
              for j in reverse 2 .. mtf_idx loop
                value (j) := value (j - 1);
              end loop;
              value (1) := selector (i);
              bits := bits + Integer_32 (mtf_idx);
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

          function Compute_Total_Cost return Natural_32 is
            --  We simulate the sending of the whole block, except the fixed costs.
            pos_countdown : Natural := group_size;
            sel_idx : Positive_32 := 1;
            cluster : Entropy_Coder_Range := selector (sel_idx);
            bits : Natural_32 := 0;
          begin
            --  Simulate the sending of the data itself:
            for mtf_idx in 1 .. mtf_last loop
              bits := bits + Natural_32 (descr (cluster)(mtf_data (mtf_idx)).bit_length);
              pos_countdown := pos_countdown - 1;
              if pos_countdown = 0 then
                pos_countdown := group_size;
                sel_idx := sel_idx + 1;
                if mtf_idx < mtf_last then
                  cluster := selector (sel_idx);
                end if;
              end if;
            end loop;
            bits := bits + Compute_Selectors_Cost + Compute_Huffman_Bit_Lengths_Cost;
            return bits;
          end Compute_Total_Cost;

          best_cost  : Natural_32 := Natural_32'Last;
          cost       : Natural_32;
          best_ec_count : Entropy_Coder_Range;
          best_max_cl   : Positive;

        begin
          --  Test various max code lengths:
          for cl_test in 9 .. 17 loop
            max_code_len := cl_test;
            --  Test each possible number of entropy coders:
            for ec_test in 2 .. max_entropy_coders loop
              entropy_coder_count := ec_test;
              Construct;
              cost := Compute_Total_Cost;
              if cost < best_cost then
                best_cost     := cost;
                best_ec_count := ec_test;
                best_max_cl   := cl_test;
              end if;
            end loop;
          end loop;
          max_code_len        := best_max_cl;
          entropy_coder_count := best_ec_count;
          Trace
            ("Max len:" & max_code_len'Image &
             ", coders:" & entropy_coder_count'Image,
             detailed);
          Construct;
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

    begin
      block_counter := block_counter + 1;
      Trace ("Block" & block_counter'Image, headlines);

      --  Data acquisition and transformation (no output):
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
