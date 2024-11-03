-------------------------
--  WORK IN PROGRESS!  --
-------------------------

with Huffman.Encoding.Length_Limited_Coding;

with Ada.Containers.Generic_Constrained_Array_Sort,
     Ada.Strings.Unbounded,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

package body BZip2.Encoding is

  procedure Encode (level : Compression_Level := 1) is
    use Interfaces;

    subtype Bit_Pos_Type is Natural range 0 .. 7;
    bit_buffer : Byte := 0;
    bit_pos : Bit_Pos_Type := 7;

    procedure Flush_Bit_Buffer is
    begin
      bit_pos := 7;
      Write_Byte (bit_buffer);
      bit_buffer := 0;
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

    --  procedure Put (b : Byte) is
    --  begin
    --    Put_Bits (Unsigned_32 (b), 8);
    --  end Put;

    procedure Put (b : Boolean) is
    begin
      Put_Bits (Boolean'Pos (b), 1);
    end Put;

    procedure Put (c : Character) is
    begin
      Put_Bits (Character'Pos (c), 8);
    end Put;

    block_capacity : constant Natural_32 := Natural_32 (level) * sub_block_size;
    block_size : Natural_32 := 0;

    type Buffer is array (Natural_32 range <>) of Byte;
    type Buffer_Access is access Buffer;

    procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Buffer, Buffer_Access);

    data : Buffer_Access;

    --  Each block is limited either by the data available
    --  (More_Bytes = False) or by the block capacity.
    --  It means that each encoding step has an end and
    --  that we can theoretically go on with the next step,
    --  perhaps at the price of using more memory.

    procedure Encode_Block is

      detailed_verbose : constant := 2;
      verbosity_level : constant := detailed_verbose;

      procedure Trace (msg : String; verbosity : Positive) is
      begin
        if verbosity_level >= verbosity then
          Ada.Text_IO.Put_Line (msg);
        end if;
      end Trace;

      procedure Trace (prefix : String; b : Buffer; verbosity : Positive) is
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

      computed_crc : Unsigned_32;

      procedure RLE_1 is

        procedure Store (x : Byte) with Inline is
        begin
          block_size := block_size + 1;
          data (block_size) := x;
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
        CRC.Init (computed_crc);
        while More_Bytes and block_size + 5 < block_capacity loop
          --  The +1 is because sometimes a pair of bytes are sent.
          b := Read_Byte;
          CRC.Update (computed_crc, b);
          if start or else b /= b_prev then
            --  Startup or Run break:
            Store_Run;
            start := False;
          elsif run = 259 then
            --  Force a break even though b = b_prev:
            Store_Run;
          else
            run := run + 1;
          end if;
          b_prev := b;
        end loop;
        Store_Run;
        if verbosity_level >= detailed_verbose then
          Trace ("RLE_1: ", data (1 .. block_size), detailed_verbose);
        end if;
      end RLE_1;

      ---------------------------------
      --  Burrows-Wheeler Transform  --
      ---------------------------------

      bwt_data : Buffer_Access;
      bwt_index : Natural_32;  --  0-based.

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

        Offset_Sort (offset.all);

        bwt_data := new Buffer (1 .. block_size);
        for i in Offset_Range loop
          --  Copy last column into transformed message:
          bwt_data (1 + i) := data (1 + (block_size - 1 - offset (i)) mod block_size);
          if offset (i) = 0 then
            --  Found the row index of the original message.
            bwt_index := i;
          end if;
        end loop;

        if verbosity_level >= detailed_verbose then
          Trace ("BWT:   ", bwt_data.all, detailed_verbose);
          Trace ("BWT index:" & bwt_index'Image, detailed_verbose);
        end if;
        Unchecked_Free (offset);
      end BWT;

      ----------------------------------------------------
      --  Move-to-Front and second Run-Length Encoding  --
      ----------------------------------------------------

      subtype Max_Alphabet is Integer range 0 .. max_alphabet_size - 1;

      type Frequency_Array is array (Max_Alphabet) of Natural_32;
      freq : Frequency_Array := (others => 0);

      type MTF_Array is array (Positive_32 range <>) of Max_Alphabet;
      type MTF_Array_Access is access MTF_Array;

      mtf_data : MTF_Array_Access;
      mtf_last : Natural_32 := 0;

      symb_in_use : constant := max_alphabet_size; --  !!  TBD: pack numbering with unseq_to_seq.

      EOB : constant := symb_in_use - 1;  --  Encoding is 0-based.

      procedure MTF_and_RLE_2 is

        procedure Store (a : Max_Alphabet) is
        begin
          mtf_last := mtf_last + 1;
          mtf_data (mtf_last) := a;
          freq (a) := freq (a) + 1;
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
      begin
        mtf_data := new MTF_Array (1 .. 2 * block_size);  --  Check real worst case capacity !!

        for i in mtf_symbol'Range loop
          mtf_symbol (i) := Byte (i);
        end loop;

        Big_MTF_RLE_2_Loop :
        for bt of bwt_data.all loop

          --  MTF part:

          Search_Value :
          for search in mtf_symbol'Range loop
            if mtf_symbol (search) = bt then
              idx := search;
              exit Search_Value;
            end if;
          end loop Search_Value;

          Move_Value_to_Front :
          for i in reverse 1 .. idx loop
            mtf_symbol (i) := mtf_symbol (i - 1);
          end loop Move_Value_to_Front;
          mtf_symbol (0) := bt;

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

      descr : Huffman.Encoding.Descriptor (Max_Alphabet);
      --  array (0 .. max_entropy_coders) of Huffman.Encoding.Descriptor (Max_Alphabet);

      procedure Entropy_Calculations is

        type Huffman_Length_Array is array (Max_Alphabet) of Natural;

        procedure LLHCL is new
          Huffman.Encoding.Length_Limited_Coding
            (Alphabet     => Max_Alphabet,  --  !! Here use the packed alphabet (using unseq_to_seq)
             Count_Type   => Natural_32,
             Count_Array  => Frequency_Array,
             Length_Array => Huffman_Length_Array,
             max_bits     => max_code_len);

        len : Huffman_Length_Array;
        --  array (0 .. max_entropy_coders) of Huffman_Length_Array;

      begin
        --  !! Here: have fun with partial sets of frequencies,
        --           the groups and the 7 possible tables !!

        --  !! TBD: pack the alphabet (unseq_to_seq)
        --          Absent this, we ballot-box-stuff the frequencies
        --          in order to have bit length >= 1.
        for a in Max_Alphabet loop
          freq (a) := freq (a) + 1;
        end loop;

        LLHCL (freq, len);
        for symbol in len'Range loop
          descr (symbol).bit_length := len (symbol);
        end loop;
        Huffman.Encoding.Prepare_Codes (descr);

        --  if verbosity_level >= detailed_verbose then
        --    for symbol in Max_Alphabet loop
        --      Trace ("Huff:" & symbol'Image & len (symbol)'Image, detailed_verbose);
        --    end loop;
        --  end if;

      end Entropy_Calculations;

      procedure Entropy_Output is
      begin
        for symbol of mtf_data (1 .. mtf_last) loop
          Put_Bits
            (Unsigned_32
              (descr (symbol).code),
               descr (symbol).bit_length);
        end loop;
      end Entropy_Output;

      procedure Put_Block_Header is
        --  pi (decimal) digits visible in hexadecimal representation!
        block_magic : constant String := "1AY&SY";
      begin
        for c of block_magic loop
          Put (c);
        end loop;
        Put_Bits (CRC.Final (computed_crc), 32);
        Put_Bits (0, 1);  --  Randomized flag, always False.
        Put_Bits (Unsigned_32 (bwt_index), 24);
      end Put_Block_Header;

      procedure Put_Block_Trees_Descriptors is

        procedure Put_Mapping_Table is
          in_use : constant array (0 .. 15) of Boolean := (others => True);  --  !!
        begin
          --  Send the first 16 bits which tell which pieces are stored.
          for i in in_use'Range loop
            Put (in_use (i));
          end loop;
          --  Send detail of the used pieces.
          for i in in_use'Range loop
            if in_use (i) then
              for j in 0 .. 15 loop
                Put (True);  --  !!
              end loop;
            end if;
          end loop;
        end Put_Mapping_Table;

        procedure Put_Selectors is
          selector_count : constant Unsigned_32 := 1 + Unsigned_32 (mtf_last) / group_size;
        begin
          Put_Bits (selector_count, 15);
          for i in 1 .. selector_count loop
            Put_Bits (0, 1);  --  MTF-transformed index for the selected entropy coder
            --  !! Output 1's for non-zero index.
          end loop;
        end Put_Selectors;

        procedure Put_Huffman_Bit_Lengths is
          current_bit_length, new_bit_length : Natural;
        begin
          --  !!  Loop over the list of used entropy coders.
          current_bit_length := descr (0).bit_length;
          Put_Bits (Unsigned_32 (current_bit_length), 5);
          for i in descr'Range loop
            new_bit_length := descr (i).bit_length;
            Adjust_Bit_length :
            loop
              if current_bit_length = new_bit_length then
                Put_Bits (0, 1);
                exit Adjust_Bit_length;
              elsif current_bit_length < new_bit_length then
                current_bit_length := current_bit_length + 1;
                Put_Bits (0, 1);
              else
                current_bit_length := current_bit_length - 1;
                Put_Bits (1, 1);
              end if;
            end loop Adjust_Bit_length;
          end loop;
        end Put_Huffman_Bit_Lengths;

      begin
        Put_Mapping_Table;
        Put_Bits (1, 3);  --  !! Number of trees (entropy coders)
        Put_Selectors;
        Put_Huffman_Bit_Lengths;
      end Put_Block_Trees_Descriptors;

    begin
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
      stream_magic : String := "BZh0";
    begin
      stream_magic (stream_magic'Last) := Character'Val (Character'Pos ('0') + level);
      for c of stream_magic loop
        Write_Byte (Character'Pos (c));  --  Last direct write.
      end loop;
    end Write_Stream_Header;

  begin
    Write_Stream_Header;
    data := new Buffer (1 .. block_capacity);
    loop
      Encode_Block;
      exit when not More_Bytes;
    end loop;
    Unchecked_Free (data);
    Flush_Bit_Buffer;
  end Encode;

end BZip2.Encoding;
