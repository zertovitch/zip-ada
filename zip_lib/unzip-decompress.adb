--  UnZip.Decompress
--------------------
--  Internal to the UnZip package. See root package (UnZip) for details & credits.

--  Legal licensing note:

--  Copyright (c) 2007 .. 2020 Gautier de Montmollin (maintainer of the Ada version)
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

with Zip.CRC_Crypto, UnZip.Decompress.Huffman, BZip2.Decoding, LZMA.Decoding;

with Ada.Exceptions, Ada.Streams.Stream_IO, Ada.Text_IO, Interfaces;

package body UnZip.Decompress is

  procedure Decompress_data (
    zip_file                   : in out Zip_Streams.Root_Zipstream_Type'Class;
    format                     : Zip.PKZip_method;
    mode                       : Write_mode;
    output_file_name           : String; -- relevant only if mode = write_to_file
    output_memory_access       : out p_Stream_Element_Array; -- \ = write_to_memory
    output_stream_access       : p_Stream;                   -- \ = write_to_stream
    feedback                   : Zip.Feedback_proc;
    explode_literal_tree       : Boolean;
    explode_slide_8KB_LZMA_EOS : Boolean;
    data_descriptor_after_data : Boolean;
    is_encrypted               : Boolean;
    password                   : in out Ada.Strings.Unbounded.Unbounded_String;
    get_new_password           : Get_password_proc;
    hint                       : in out Zip.Headers.Local_File_Header
  )
  is
    --  Disable AdaControl rule for detecting global variables, they have become local here.
    --## RULE OFF Directly_Accessed_Globals
    --
    --  I/O Buffers: Size of input buffer
    inbuf_size : constant := 16#8000#;  --  (orig: 16#1000# B =  4 KiB)
    --  I/O Buffers: Size of sliding dictionary and output buffer
    wsize     : constant := 16#10000#;  --  (orig: 16#8000# B = 32 KiB)

    ----------------------------------------------------------------------------
    -- Specifications of UnZ_* packages (remain of Info Zip's code structure) --
    ----------------------------------------------------------------------------
    use Ada.Exceptions, Interfaces;

    package UnZ_Glob is -- Not global anymore, since local to Decompress_data :-)
      --  I/O Buffers: Sliding dictionary for unzipping, and output buffer as well
      slide : Zip.Byte_Buffer (0 .. wsize);
      slide_index : Integer := 0;  --  Current Position in slide
      --  I/O Buffers: Input buffer
      inbuf : Zip.Byte_Buffer (0 .. inbuf_size - 1);
      inpos, readpos : Integer;  --  pos. in input buffer, pos. read from file
      compsize,            --  compressed size of file
      reachedsize,         --  number of bytes read from zipfile
      uncompsize,          --  uncompressed size of file
      effective_writes : Zip.Zip_32_Data_Size_Type;
      --  ^ count of effective bytes written or tested, for feedback only
      percents_done    : Natural;
      crc32val : Unsigned_32;  -- crc calculated from data
      uncompressed_index  : Ada.Streams.Stream_Element_Offset;
    end UnZ_Glob;

    Zip_EOF  : Boolean; -- read over end of zip section for this file
    LZ77_dump : Ada.Text_IO.File_Type;

    package UnZ_IO is
      out_bin_file : Ada.Streams.Stream_IO.File_Type;
      out_txt_file : Ada.Text_IO.File_Type;
      last_char    : Character := ' ';

      procedure Init_Buffers;

      procedure Read_byte_no_decrypt (bt : out Zip.Byte);
        pragma Inline (Read_byte_no_decrypt);

      function Read_byte_decrypted return Unsigned_8;  --  NB: reading goes on a while even if
        pragma Inline (Read_byte_decrypted);           --  Zip_EOF is set: just gives garbage

      package Bit_buffer is
        procedure Init;
        --  Read at least n bits into the bit buffer, returns the n first bits
        function Read (n : Natural) return Integer;
          pragma Inline (Read);
        function Read_U32 (n : Natural) return Unsigned_32;
          pragma Inline (Read_U32);
        --  Inverts (NOT operator) the result before masking by n bits
        function Read_inverted (n : Natural) return Integer;
          pragma Inline (Read_inverted);
        --  Dump n bits no longer needed from the bit buffer
        procedure Dump (n : Natural);
          pragma Inline (Dump);
        procedure Dump_to_byte_boundary;
        function Read_and_dump (n : Natural) return Integer;
          pragma Inline (Read_and_dump);
        function Read_and_dump_U32 (n : Natural) return Unsigned_32;
          pragma Inline (Read_and_dump_U32);
      end Bit_buffer;

      procedure Flush (x : Natural);  --  directly from slide to output stream

      procedure Flush_if_full (W : in out Integer; unflushed : in out Boolean);
      pragma Inline (Flush_if_full);

      procedure Flush_if_full (W : in out Integer);
      pragma Inline (Flush_if_full);

      procedure Copy (distance, copy_length : Natural; index : in out Natural);
      pragma Inline (Copy);

      procedure Copy_or_zero (
        distance, length :        Natural;
        index            : in out Natural;
        unflushed        : in out Boolean);
      pragma Inline (Copy_or_zero);

      procedure Delete_output;  --  an error has occured (bad compressed data)

    end UnZ_IO;

    package UnZ_Meth is
      procedure Copy_stored;
      procedure Unshrink;
      subtype Reduction_factor is Integer range 1 .. 4;
      procedure Unreduce (factor : Reduction_factor);
      procedure Explode (literal_tree, slide_8_KB : Boolean);
      deflate_e_mode : Boolean := False;
      procedure Inflate;
      procedure Bunzip2;      --  Nov-2009
      procedure LZMA_Decode;  --  Jun-2014
    end UnZ_Meth;

    procedure Process_feedback (new_bytes : Zip.Zip_32_Data_Size_Type) is
    pragma Inline (Process_feedback);
      new_percents_done : Natural;
      user_aborting : Boolean;
      use Zip;
    begin
      if feedback = null or UnZ_Glob.uncompsize = 0 then
        return; -- no feedback proc. or cannot calculate percentage
      end if;
      UnZ_Glob.effective_writes := UnZ_Glob.effective_writes + new_bytes;
      new_percents_done := Natural (
        (100.0 * Float (UnZ_Glob.effective_writes)) / Float (UnZ_Glob.uncompsize)
      );
      if new_percents_done > UnZ_Glob.percents_done then
        feedback (
          percents_done => new_percents_done,
          entry_skipped => False,
          user_abort    => user_aborting
        );
        if user_aborting then
          raise User_abort;
        end if;
        UnZ_Glob.percents_done := new_percents_done;
      end if;
    end Process_feedback;

    use Zip.CRC_Crypto;
    local_crypto_pack : Crypto_pack;

    ------------------------------
    -- Bodies of UnZ_* packages --
    ------------------------------
    package body UnZ_IO is

      procedure Init_Buffers is
      begin
        UnZ_Glob.inpos   :=  0;  --  Input buffer position
        UnZ_Glob.readpos := -1;  --  Nothing read
        UnZ_Glob.slide_index := 0;
        UnZ_Glob.reachedsize      := 0;
        UnZ_Glob.effective_writes := 0;
        UnZ_Glob.percents_done    := 0;
        Zip_EOF := False;
        Zip.CRC_Crypto.Init (UnZ_Glob.crc32val);
        Bit_buffer.Init;
      end Init_Buffers;

      procedure Process_compressed_end_reached is
      begin
        if Zip_EOF then  --  We came already here once
          raise Zip.Archive_corrupted with
            "Decoding went past compressed data size plus one buffer length";
          --  Avoid infinite loop on data with exactly buffer's length and no end marker
        else
          UnZ_Glob.readpos := UnZ_Glob.inbuf'Length;
          --  Simulates reading -> no blocking.
          --  The buffer is full of "random" data and we hope for a wrong code or a CRC error
          Zip_EOF := True;
        end if;
      end Process_compressed_end_reached;

      procedure Read_buffer is
      begin
        if full_trace then
          Ada.Text_IO.Put ("[Read_buffer...");
        end if;
        if UnZ_Glob.reachedsize > UnZ_Glob.compsize + 2 then
          --  +2: last code is smaller than requested!
          Process_compressed_end_reached;
        else
          begin
            Zip.Block_Read (
              stream        => zip_file,
              buffer        => UnZ_Glob.inbuf,
              actually_read => UnZ_Glob.readpos
            );
          exception
            when others => -- I/O error
              Process_compressed_end_reached;
          end;
          if UnZ_Glob.readpos = 0 then -- No byte at all was read
            Process_compressed_end_reached;
          end if;
          UnZ_Glob.reachedsize :=
            UnZ_Glob.reachedsize + Zip.Zip_32_Data_Size_Type (UnZ_Glob.readpos);
          UnZ_Glob.readpos := UnZ_Glob.readpos - 1;  --  Reason: index of inbuf starts at 0
        end if;
        UnZ_Glob.inpos := 0;
        if full_trace then
          Ada.Text_IO.Put_Line ("finished]");
        end if;
      end Read_buffer;

      procedure Read_byte_no_decrypt (bt : out Zip.Byte) is
      begin
        if UnZ_Glob.inpos > UnZ_Glob.readpos then
          Read_buffer;
        end if;
        bt := UnZ_Glob.inbuf (UnZ_Glob.inpos);
        UnZ_Glob.inpos := UnZ_Glob.inpos + 1;
      end Read_byte_no_decrypt;

      function Read_byte_decrypted return Unsigned_8 is
        bt : Zip.Byte;
      begin
        Read_byte_no_decrypt (bt);
        Decode (local_crypto_pack, bt);
        return bt;
      end Read_byte_decrypted;

      package body Bit_buffer is
        B : Unsigned_32;
        K : Integer;

        procedure Init is
        begin
          B := 0;
          K := 0;
        end Init;

        procedure Need (n : Natural) is
          pragma Inline (Need);
        begin
          while K < n loop
            B := B or Shift_Left (Unsigned_32 (Read_byte_decrypted), K);
            K := K + 8;
          end loop;
        end Need;

        procedure Dump (n : Natural) is
        begin
          B := Shift_Right (B, n);
          K := K - n;
        end Dump;

        procedure Dump_to_byte_boundary is
        begin
          Dump (K mod 8);
        end Dump_to_byte_boundary;

        function Read_U32 (n : Natural) return Unsigned_32 is
        begin
          Need (n);
          return B and (Shift_Left (1, n) - 1);
        end Read_U32;

        function Read_inverted (n : Natural) return Integer is
        begin
          Need (n);
          return Integer ((not B) and (Shift_Left (1, n) - 1));
        end Read_inverted;

        function Read (n : Natural) return Integer is
        begin
          return Integer (Read_U32 (n));
        end Read;

        function Read_and_dump (n : Natural) return Integer is
          res : Integer;
        begin
          res := Read (n);
          Dump (n);
          return res;
        end Read_and_dump;

        function Read_and_dump_U32 (n : Natural) return Unsigned_32 is
          res : Unsigned_32;
        begin
          res := Read_U32 (n);
          Dump (n);
          return res;
        end Read_and_dump_U32;

      end Bit_buffer;

      procedure Flush (x : Natural) is
        use Zip, Ada.Streams;
      begin
        if full_trace then
          Ada.Text_IO.Put ("[Flush...");
        end if;
        begin
          case mode is
            when write_to_binary_file =>
              Block_Write (Ada.Streams.Stream_IO.Stream (out_bin_file).all, UnZ_Glob.slide (0 .. x - 1));
            when write_to_text_file =>
              Zip.Write_as_text (
                UnZ_IO.out_txt_file, UnZ_Glob.slide (0 .. x - 1), UnZ_IO.last_char
              );
            when write_to_memory =>
              for i in 0 .. x - 1 loop
                output_memory_access (UnZ_Glob.uncompressed_index) :=
                  Ada.Streams.Stream_Element (UnZ_Glob.slide (i));
                UnZ_Glob.uncompressed_index := UnZ_Glob.uncompressed_index + 1;
              end loop;
            when write_to_stream =>
              Block_Write (output_stream_access.all, UnZ_Glob.slide (0 .. x - 1));
            when just_test =>
              null;
          end case;
        exception
          when others =>
            raise UnZip.Write_Error;
        end;
        Zip.CRC_Crypto.Update (UnZ_Glob.crc32val, UnZ_Glob.slide (0 .. x - 1));
        Process_feedback (Zip_32_Data_Size_Type (x));
        if full_trace then
          Ada.Text_IO.Put_Line ("finished]");
        end if;
      end Flush;

      procedure Flush_if_full (W : in out Integer; unflushed : in out Boolean) is
      begin
        if W = wsize then
          Flush (wsize);
          W := 0;
          unflushed := False;
        end if;
      end Flush_if_full;

      procedure Flush_if_full (W : in out Integer) is
      begin
        if W = wsize then
          Flush (wsize);
          W := 0;
        end if;
      end Flush_if_full;

      ----------------------------------------------------
      -- Reproduction of sequences in the output slide. --
      ----------------------------------------------------

      --  Internal:

      procedure Adjust_to_Slide (
          source         : in out Integer;
          remain         : in out Natural;
          part           :    out Integer;
          index          :        Integer)
      is
        pragma Inline (Adjust_to_Slide);
      begin
        source := source mod wsize;
        --  source and index are now in 0 .. WSize-1
        if source > index then
          part := wsize - source;
        else
          part := wsize - index;
        end if;
        --  NB: part is in 1..WSize (part cannot be 0)
        if part > remain then
          part := remain;
        end if;
        --  Now part <= remain
        remain := remain - part;
        --  NB: remain cannot be < 0
      end Adjust_to_Slide;

      procedure Copy_range (source, index : in out Natural; amount : Positive) is
        pragma Inline (Copy_range);
      begin
        if full_trace then
          Ada.Text_IO.Put (
            "(Copy_range: source=" & Integer'Image (source) &
            " index=" & Integer'Image (index) &
            " amount=" & Integer'Image (amount));
        end if;
        if abs (index - source) < amount then
          if full_trace and then source < index then
            Ada.Text_IO.Put (
              "; replicates" &
              Integer'Image (amount) & " /" & Integer'Image (index - source) &
              " )"
            );
            --  ...times the range source..index-1
          end if;
          --  if source >= index, the effect of copy is just like the non-overlapping case
          for count in reverse 1 .. amount loop
            UnZ_Glob.slide (index) := UnZ_Glob.slide (source);
            index  := index  + 1;
            source := source + 1;
          end loop;
        else  --  non-overlapping -> copy slice
          UnZ_Glob.slide (index .. index + amount - 1) :=
            UnZ_Glob.slide (source .. source + amount - 1);
          index  := index  + amount;
          source := source + amount;
        end if;
        if full_trace then
          Ada.Text_IO.Put (')');
        end if;
      end Copy_range;

      --  The copying routines:

      procedure Copy (distance, copy_length : Natural; index : in out Natural) is
        source, part, remain : Integer;
      begin
        if some_trace then
          Ada.Text_IO.Put_Line (LZ77_dump, "DLE" & Integer'Image (distance) & Integer'Image (copy_length));
        end if;
        source := index - distance;
        remain := copy_length;
        loop
          Adjust_to_Slide (source, remain, part, index);
          Copy_range (source, index, part);
          Flush_if_full (index);
          exit when remain = 0;
        end loop;
      end Copy;

      procedure Copy_or_zero (
          distance, length :        Natural;
          index            : in out Natural;
          unflushed        : in out Boolean)
      is
        source, part, remain : Integer;
      begin
        source := index - distance;
        remain := length;
        loop
          Adjust_to_Slide (source, remain, part, index);
          if unflushed and then index <= source then
            UnZ_Glob.slide (index .. index + part - 1) := (others => 0);
            index  := index  + part;
            source := source + part;
          else
            Copy_range (source, index, part);
          end if;
          Flush_if_full (index, unflushed);
          exit when remain = 0;
        end loop;
      end Copy_or_zero;

      procedure Delete_output is -- an error has occured (bad compressed data)
      begin
        if no_trace then -- if there is a trace, we are debugging
          case mode is   --  and want to keep the malformed file
            when write_to_binary_file =>
              Ada.Streams.Stream_IO.Delete (UnZ_IO.out_bin_file);
            when write_to_text_file =>
              Ada.Text_IO.Delete (UnZ_IO.out_txt_file);
            when write_to_memory | write_to_stream | just_test =>
              null; -- Nothing to delete!
          end case;
        end if;
      end Delete_output;

    end UnZ_IO;

    procedure Init_Decryption (password_for_keys : String; crc_check : Unsigned_32) is
      c : Zip.Byte := 0;
      t : Unsigned_32;
    begin
      --  Step 1 - Initializing the encryption keys
      Init_keys (local_crypto_pack, password_for_keys);
      --  Step 2 - Decrypting the encryption header. 11 bytes are random,
      --           just to shuffle the keys, 1 byte is from the CRC value.
      Set_mode (local_crypto_pack, encrypted);
      for i in 1 .. 12 loop
        UnZ_IO.Read_byte_no_decrypt (c);
        Decode (local_crypto_pack, c);
      end loop;
      t := Zip_Streams.Calendar.Convert (hint.file_timedate);
      --  Last byte used to check password; 1/256 probability of success with any password!
      if c /= Zip.Byte (Shift_Right (crc_check, 24)) and not
        --  Dec. 2012. This is a feature of Info-Zip (crypt.c), not of PKWARE.
        --  Since CRC is only known at the end of a one-way stream
        --  compression, and cannot be written back, they are using a byte of
        --  the time stamp instead. This is NOT documented in PKWARE's appnote.txt v.6.3.3.
        (data_descriptor_after_data and c = Zip.Byte (Shift_Right (t, 8) and 16#FF#))
      then
        raise UnZip.Wrong_password;
      end if;
    end Init_Decryption;

    package body UnZ_Meth is

      --------[ Method: Unshrink ]--------

      --  Original in Pascal written by Christian Ghisler.

      Initial_Code_Size : constant := 9;
      Maximum_Code_Size : constant := 13;
      Max_Code          : constant := 2 ** Maximum_Code_Size;
      Max_Stack         : constant := 2 ** Maximum_Code_Size;

      --  Rest of slide=write buffer =766 bytes

      Write_Max : constant := wsize - 3 * (Max_Code - 256) - Max_Stack - 2;

      Next_Free : Integer;      --  Next free code in trie
      Write_Ptr : Integer;      --  Pointer to output buffer

      Writebuf : Zip.Byte_Buffer (0 .. Write_Max);  --  Write buffer

      procedure Unshrink_Flush is
        use Zip, Ada.Streams, Ada.Streams.Stream_IO;
      begin
        if full_trace then
          Ada.Text_IO.Put ("[Unshrink_Flush]");
        end if;
        begin
          case mode is
            when write_to_binary_file =>
              Block_Write (Stream (UnZ_IO.out_bin_file).all, Writebuf (0 .. Write_Ptr - 1));
            when write_to_text_file =>
              Zip.Write_as_text (UnZ_IO.out_txt_file, Writebuf (0 .. Write_Ptr - 1), UnZ_IO.last_char);
            when write_to_memory =>
              for I in 0 .. Write_Ptr - 1 loop
                output_memory_access (UnZ_Glob.uncompressed_index) :=
                  Stream_Element (Writebuf (I));
                UnZ_Glob.uncompressed_index :=  UnZ_Glob.uncompressed_index + 1;
              end loop;
            when write_to_stream =>
              Block_Write (output_stream_access.all, Writebuf (0 .. Write_Ptr - 1));
            when just_test =>
              null;
          end case;
        exception
          when others =>
            raise UnZip.Write_Error;
        end;
        Zip.CRC_Crypto.Update (UnZ_Glob.crc32val, Writebuf (0 .. Write_Ptr - 1));
        Process_feedback (Zip_32_Data_Size_Type (Write_Ptr));
      end Unshrink_Flush;

      procedure UD_Write_Byte (B : Zip.Byte) is
      begin
        Writebuf (Write_Ptr) := B;
        Write_Ptr := Write_Ptr + 1;
        if Write_Ptr > Write_Max then
          Unshrink_Flush;
          Write_Ptr := 0;
        end if;
      end UD_Write_Byte;

      procedure Unshrink is
        S : Zip.Zip_32_Data_Size_Type := UnZ_Glob.uncompsize;

        Last_Incode     : Integer;
        Last_Outcode    : Zip.Byte;
        Code_Size       : Integer := Initial_Code_Size;  --  Actual code size [9 .. 13]
        Actual_Max_Code : Integer;  --  Max code to be searched for leaf nodes
        First_Entry     : constant := 257;
        Previous_Code   : array (First_Entry .. Max_Code) of Integer;
        Stored_Literal  : array (First_Entry .. Max_Code) of Zip.Byte;

        procedure Clear_Leaf_Nodes is
          Is_Leaf : array (First_Entry .. Max_Code) of Boolean := (others => True);
          Pc : Integer;  --  Previous code
        begin
          if full_trace then
            Ada.Text_IO.Put ("[Clear leaf nodes @ pos" &
              Zip.Zip_32_Data_Size_Type'Image (UnZ_Glob.uncompsize - S) &
              "; old Next_Free =" & Integer'Image (Next_Free));
          end if;
          for I in First_Entry .. Actual_Max_Code loop
            Pc := Previous_Code (I);
            if  Pc > 256 then
              --  Pc is in a tree as well
              Is_Leaf (Pc) := False;
            end if;
          end loop;

          --  Build new free list
          Pc := -1;
          Next_Free := -1;
          for I in First_Entry .. Actual_Max_Code loop
            --  Either free before, or marked now as leaf
            if Previous_Code (I) < 0 or Is_Leaf (I) then
              --  Link last item to this item
              if Pc = -1 then
                Next_Free := I;
              else
                --  Next free node from Pc is I.
                Previous_Code (Pc) := -I;
              end if;
              Pc := I;
            end if;
          end loop;

          if Pc /= -1 then
            --  Last (old or new) free node points to the first "never used".
            Previous_Code (Pc) := -(Actual_Max_Code + 1);
          end if;
          if Next_Free = -1 then
            --  Unlikely but possible case:
            --     - no previously free or leaf node found, or
            --     - table clearing is ordered when the table is still empty.
            Next_Free := Actual_Max_Code + 1;
          end if;

          if full_trace then
            Ada.Text_IO.Put ("; new Next_Free =" & Integer'Image (Next_Free) & ']');
          end if;
        end Clear_Leaf_Nodes;

        procedure Attempt_Table_Increase is
          Candidate : constant Integer := Next_Free;
        begin
          if Candidate > Max_Code then
            --  This case is supported by PKZip's LZW variant.
            --  Table clearing is done only on a special command.
            if some_trace then
              Ada.Text_IO.Put ("[Table is full]");
            end if;
          else
            if Candidate not in Previous_Code'Range then
              raise Zip.Archive_corrupted with "Wrong LZW (Shrink) index";
            end if;
            Next_Free := -Previous_Code (Candidate);
            Actual_Max_Code := Integer'Max (Actual_Max_Code, Next_Free - 1);

            --  Next node in free list
            Previous_Code (Candidate)  := Last_Incode;
            Stored_Literal (Candidate) := Last_Outcode;
          end if;
        end Attempt_Table_Increase;

        Incode    : Integer;  --  Code read in
        New_Code  : Integer;  --  Save new normal code read
        Stack     : Zip.Byte_Buffer (0 .. Max_Stack);  --  Stack for output
        Stack_Ptr : Integer := Max_Stack;

        --  PKZip's Shrink is a variant of the LZW algorithm in that the
        --  compressor controls the code increase and the table clearing.
        --  See appnote.txt, section 5.1.
        Special_Code : constant := 256;
        Code_for_increasing_code_size : constant := 1;
        Code_for_clearing_table       : constant := 2;

        procedure Read_Code is
          pragma Inline (Read_Code);
        begin
          Incode := UnZ_IO.Bit_buffer.Read_and_dump (Code_Size);
        end Read_Code;

      begin
        --  Initialize free codes list
        for I in Previous_Code'Range loop
          Previous_Code (I) := -(I + 1);
        end loop;
        --
        Stored_Literal := (others => 0);
        Stack          := (others => 0);
        Writebuf       := (others => 0);

        if UnZ_Glob.compsize = Zip.Zip_32_Data_Size_Type'Last then
          --  Compressed Size was not in header!
          raise UnZip.Not_supported;
        elsif UnZ_Glob.uncompsize = 0 then
          return;  --  compression of a 0-file with Shrink.pas
        end if;

        Next_Free := First_Entry;
        Actual_Max_Code := First_Entry - 1;
        Write_Ptr := 0;

        Read_Code;
        Last_Incode := Incode;
        if Incode not in 0 .. 255 then
          raise Zip.Archive_corrupted with "Wrong LZW (Shrink) 1st byte; must be a literal";
        end if;
        Last_Outcode := Zip.Byte (Incode);
        UD_Write_Byte (Last_Outcode);
        S := S - 1;

        Main_Unshrink_Loop :
        while S > 0 and then not Zip_EOF loop
          Read_Code;
          if Incode = Special_Code then  --  Code = 256
            Read_Code;
            case Incode is
              when Code_for_increasing_code_size =>
                Code_Size := Code_Size + 1;
                if some_trace then
                  Ada.Text_IO.Put (
                    "[Increment LZW code size to" & Integer'Image (Code_Size) &
                    " bits @ pos" & Zip.Zip_32_Data_Size_Type'Image (UnZ_Glob.uncompsize - S) & ']'
                  );
                end if;
                if Code_Size > Maximum_Code_Size then
                  raise Zip.Archive_corrupted with "Wrong LZW (Shrink) code size";
                end if;
              when Code_for_clearing_table =>
                Clear_Leaf_Nodes;
              when others =>
                raise Zip.Archive_corrupted with
                  "Wrong LZW (Shrink) special code" & Integer'Image (Incode);
            end case;
          else  --  Normal code (either a literal (< 256), or a tree node (> 256))
            New_Code := Incode;
            if Incode < 256 then          --  Literal (simple character)
              Last_Outcode :=  Zip.Byte (Incode);
              UD_Write_Byte (Last_Outcode);
              S := S - 1;
            else  --  Tree node (code > 256)
              if Previous_Code (Incode) < 0 then
                --  First node is orphan (parent is a free node).
                if full_trace then
                  Ada.Text_IO.Put ("[ Node from stream is orphan ]");
                end if;
                Stack (Stack_Ptr) := Last_Outcode;
                Stack_Ptr := Stack_Ptr - 1;
                Incode := Last_Incode;
              end if;
              while Incode > 256 loop
                if Stack_Ptr < Stack'First then
                  raise Zip.Archive_corrupted with "LZW (Shrink): String stack exhausted";
                end if;
                if Incode > Max_Code then
                  raise Zip.Archive_corrupted with "LZW (Shrink): Incode out of range";
                end if;
                if Previous_Code (Incode) < 0 then
                  --  Linked node is orphan (parent is a free node).
                  --  This rare case appears on some data, compressed only by PKZIP.
                  --  The last PKZIP version known to us that is able to compress
                  --  with the Shrink algorithm is PKZIP v.1.10, 1990-03-15.
                  if some_trace then
                    Ada.Text_IO.Put ("[ Linked node is orphan ]");
                  end if;
                  Stack (Stack_Ptr) := Last_Outcode;
                  Incode := Last_Incode;
                else
                  Stack (Stack_Ptr) := Stored_Literal (Incode);
                  Incode := Previous_Code (Incode);
                end if;
                Stack_Ptr := Stack_Ptr - 1;
              end loop;
              --  NB: Incode cannot be negative (orphan case treated above).
              --      It is <= 256 because of the while loop.
              --      It is /= 256 because it is set to a Last_Incode value (directly or
              --        through Previous_Code) which is either in [0 .. 255] or > 256.
              --      So Incode is in [0 .. 255].
              Last_Outcode := Zip.Byte (Incode);
              UD_Write_Byte (Last_Outcode);
              --  Now we output the string in forward order.
              for I in Stack_Ptr + 1 .. Max_Stack  loop
                UD_Write_Byte (Stack (I));
              end loop;
              S := S - Zip.Zip_32_Data_Size_Type (Max_Stack - Stack_Ptr + 1);
              Stack_Ptr := Max_Stack;
            end if;
            Attempt_Table_Increase;
            Last_Incode := New_Code;
          end if;
        end loop Main_Unshrink_Loop;

        if some_trace then
          Ada.Text_IO.Put ("[ Unshrink main loop finished ]");
        end if;
        Unshrink_Flush;
      end Unshrink;

      --------[ Method: Unreduce ]--------

      procedure Unreduce (factor : Reduction_factor) is

        --  Original slide limit: 16#4000#
        DLE_code : constant := 144;
        subtype Symbol_range is Integer range 0 .. 255;
        subtype Follower_range is Integer range 0 .. 63;  --  Appnote: <= 32 !
        Followers : array (Symbol_range, Follower_range) of Symbol_range :=
          (others => (others => 0));
        Slen : array (Symbol_range) of Follower_range;

        --  Bits taken by (x-1) mod 256:
        B_Table : constant array (Symbol_range) of Integer :=
            (0        => 8,
             1 .. 2   => 1,
             3 .. 4   => 2,
             5 .. 8   => 3,
             9 .. 16  => 4,
            17 .. 32  => 5,
            33 .. 64  => 6,
            65 .. 128 => 7,
           129 .. 255 => 8);

        procedure LoadFollowers is
          list_followers : constant Boolean := some_trace;
          procedure Show_symbol (S : Symbol_range) is
          begin
            if S in 32 .. 254 then
              Ada.Text_IO.Put (Character'Val (S));
            else
              Ada.Text_IO.Put ('{' & Symbol_range'Image (S) & '}');
            end if;
          end Show_symbol;
        begin
          for X in reverse Symbol_range loop
            Slen (X) := UnZ_IO.Bit_buffer.Read_and_dump (6);
            if list_followers then
              Show_symbol (X);
              Ada.Text_IO.Put (" -> (" & Integer'Image (Slen (X)) & ") ");
            end if;
            for I in 0 .. Slen (X) - 1  loop
              Followers (X, I) := UnZ_IO.Bit_buffer.Read_and_dump (8);
              if list_followers then
                Show_symbol (Followers (X, I));
              end if;
            end loop;
            if list_followers then
              Ada.Text_IO.New_Line;
            end if;
          end loop;
        end LoadFollowers;

        length,
        char_read,
        last_char : Integer := 0;
        --  ^ some := 0 are useless, just to calm down ObjectAda 7.2.2
        S : Zip.Zip_32_Data_Size_Type := UnZ_Glob.uncompsize;
        --  number of bytes left to decompress
        unflushed : Boolean := True;
        maximum_AND_mask : constant Unsigned_32 := Shift_Left (1, 8 - factor) - 1;

        procedure Out_byte (b : Zip.Byte) is
        begin
          S := S - 1;
          UnZ_Glob.slide (UnZ_Glob.slide_index) := b;
          UnZ_Glob.slide_index := UnZ_Glob.slide_index + 1;
          UnZ_IO.Flush_if_full (UnZ_Glob.slide_index, unflushed);
        end Out_byte;

        V : Unsigned_32 := 0;
        type State_type is (normal, length_a, length_b, distance);
        state : State_type := normal;

      begin
        LoadFollowers;

        while S > 0 and then not Zip_EOF loop

          --  1/ Probabilistic expansion
          if Slen (last_char) = 0 then
            --  follower set is empty for this character
            char_read := UnZ_IO.Bit_buffer.Read_and_dump (8);
          elsif UnZ_IO.Bit_buffer.Read_and_dump (1) = 0  then
            char_read := Followers (
              last_char,
              UnZ_IO.Bit_buffer.Read_and_dump (B_Table (Slen (last_char)))
            );
          else
            char_read := UnZ_IO.Bit_buffer.Read_and_dump (8);
          end if;

          --  2/ Expand the resulting Zip.Byte into repeated sequences
          case state is

            when normal =>
              if char_read = DLE_code then
                --  >> Next will be a DLE
                state := length_a;
              else
                --  >> A single char
                Out_byte (Zip.Byte (char_read));
              end if;

            when length_a =>
              if char_read = 0 then
                --  >> DLE_code & 0 -> was just the Zip.Byte coded DLE_code
                Out_byte (DLE_code);
                state := normal;
              else
                V := Unsigned_32 (char_read);
                length := Integer (V and maximum_AND_mask);
                --  The remaining bits of V will be used for the distance
                if length = Integer (maximum_AND_mask) then
                  state := length_b;
                  --  >> length must be completed before reading distance
                else
                  state := distance;
                end if;
              end if;

            when length_b =>
              length := length + char_read;
              state := distance;

            when distance =>
              length := length + 3;
              S := S - Zip.Zip_32_Data_Size_Type (length);

              UnZ_IO.Copy_or_zero (
                distance   => char_read + 1 + Integer (Shift_Right (V, 8 - factor) * 2**8),
                length     => length,
                index      => UnZ_Glob.slide_index,
                unflushed  => unflushed
              );
              state := normal;

          end case;

          last_char := char_read;  -- store character for next iteration
        end loop;

        UnZ_IO.Flush (UnZ_Glob.slide_index);
      end Unreduce;

      --------[ Method: Explode ]--------

      --  C code by info-zip group, translated to Pascal by Christian Ghisler
      --  based on unz51g.zip

      use UnZip.Decompress.Huffman;

      procedure Get_Tree (L : out Length_array) is
        I, K, J, B : Unsigned_32;
        N          : constant Unsigned_32 := L'Length;
        L_Idx      : Integer    := L'First;
      begin
        if full_trace then
          Ada.Text_IO.Put_Line ("Begin UnZ_Expl.Get_tree");
        end if;

        I := Unsigned_32 (UnZ_IO.Read_byte_decrypted) + 1;
        K := 0;

        loop
          J := Unsigned_32 (UnZ_IO.Read_byte_decrypted);
          B := (J  and  16#0F#) + 1;
          J := (J  and  16#F0#) / 16 + 1;
          if  K + J > N then
            raise Zip.Archive_corrupted;
          end if;

          loop
            L (L_Idx) := Natural (B);
            L_Idx := L_Idx + 1;
            K := K + 1;
            J := J - 1;
            exit when  J = 0;
          end loop;

          I := I - 1;
          exit when  I = 0;
        end loop;

        if  K /= N then
          raise Zip.Archive_corrupted;
        end if;

        if full_trace then
          Ada.Text_IO.Put_Line ("End   UnZ_Expl.Get_tree");
        end if;
      end Get_Tree;

      procedure Explode_Lit ( -- method with 3 trees
        Needed : Integer;
        Tb, Tl, Td : p_Table_list;
        Bb, Bl, Bd : Integer
      )
      is
        S       : Unsigned_32;
        E, N, D : Integer;

        W : Integer := 0;
        Ct : p_HufT_table; -- current table
        Ci : Natural;                               -- current index
        unflushed : Boolean := True; -- true while slide not yet unflushed

      begin
        if full_trace then
          Ada.Text_IO.Put_Line ("Begin Explode_lit");
        end if;

        UnZ_IO.Bit_buffer.Init;

        S := UnZ_Glob.uncompsize;
        while  S > 0  and  not Zip_EOF  loop
          if UnZ_IO.Bit_buffer.Read_and_dump (1) /= 0 then  -- 1: Literal
            S := S - 1;
            Ct := Tb.table;
            Ci := UnZ_IO.Bit_buffer.Read_inverted (Bb);

            loop
              E :=  Ct (Ci).extra_bits;
              exit when E <= 16;

              if E = invalid then
                raise Zip.Archive_corrupted;
              end if;

              UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);
              E := E - 16;
              Ct := Ct (Ci).next_table;
              Ci := UnZ_IO.Bit_buffer.Read_inverted (E);
            end loop;

            UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);
            UnZ_Glob.slide (W) :=  Zip.Byte (Ct (Ci).n);
            W := W + 1;
            UnZ_IO.Flush_if_full (W, unflushed);

          else                                       -- 0: Copy
            D := UnZ_IO.Bit_buffer.Read_and_dump (Needed);
            Ct := Td.table;
            Ci := UnZ_IO.Bit_buffer.Read_inverted (Bd);

            loop
              E := Ct (Ci).extra_bits;
              exit when  E <= 16;

              if E = invalid then
                raise Zip.Archive_corrupted;
              end if;

              UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);
              E := E - 16;
              Ct := Ct (Ci).next_table;
              Ci := UnZ_IO.Bit_buffer.Read_inverted (E);
            end loop;

            UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);
            D := D + Ct (Ci).n;

            Ct := Tl.table;
            Ci := UnZ_IO.Bit_buffer.Read_inverted (Bl);

            loop
              E := Ct (Ci).extra_bits;
              exit when  E <= 16;

              if E = invalid then
                raise Zip.Archive_corrupted;
              end if;

              UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);
              E := E - 16;
              Ct := Ct (Ci).next_table;
              Ci := UnZ_IO.Bit_buffer.Read_inverted (E);
            end loop;

            UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);

            N :=  Ct (Ci).n;
            if E /= 0 then
              N := N + UnZ_IO.Bit_buffer.Read_and_dump (8);
            end if;
            S := S - Unsigned_32 (N);

            UnZ_IO.Copy_or_zero (
              distance   => D,
              length     => N,
              index      => W,
              unflushed  => unflushed
            );

          end if;
        end loop;

        UnZ_IO.Flush (W);
        if Zip_EOF then
          raise Zip.Archive_corrupted with "End of stream reached";
        end if;

        if full_trace then
          Ada.Text_IO.Put_Line ("End   Explode_lit");
        end if;
      end Explode_Lit;

      procedure Explode_Nolit ( -- method with 2 trees
          Needed : Integer;
          Tl, Td : p_Table_list;
          Bl, Bd : Integer
      )
      is
        S       : Unsigned_32;
        E, N, D : Integer;
        W : Integer := 0;
        Ct : p_HufT_table; -- current table
        Ci : Natural;                               -- current index
        unflushed : Boolean := True; -- true while slide not yet unflushed

      begin
        if full_trace then
          Ada.Text_IO.Put_Line ("Begin Explode_nolit");
        end if;

        UnZ_IO.Bit_buffer.Init;
        S := UnZ_Glob.uncompsize;
        while  S > 0  and not Zip_EOF  loop
          if UnZ_IO.Bit_buffer.Read_and_dump (1) /= 0 then  -- 1: Literal
            S := S - 1;
            UnZ_Glob.slide (W) :=
              Zip.Byte (UnZ_IO.Bit_buffer.Read_and_dump (8));
            W := W + 1;
            UnZ_IO.Flush_if_full (W, unflushed);
          else                                       -- 0: Copy
            D := UnZ_IO.Bit_buffer.Read_and_dump (Needed);
            Ct := Td.table;
            Ci := UnZ_IO.Bit_buffer.Read_inverted (Bd);

            loop
              E := Ct (Ci).extra_bits;
              exit when  E <= 16;

              if E = invalid then
                raise Zip.Archive_corrupted;
              end if;

              UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);
              E := E - 16;
              Ct := Ct (Ci).next_table;
              Ci := UnZ_IO.Bit_buffer.Read_inverted (E);
            end loop;

            UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);

            D :=  D + Ct (Ci).n;
            Ct := Tl.table;
            Ci := UnZ_IO.Bit_buffer.Read_inverted (Bl);

            loop
              E := Ct (Ci).extra_bits;
              exit when E <= 16;

              if E = invalid then
                raise Zip.Archive_corrupted;
              end if;

              UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);
              E := E - 16;
              Ct := Ct (Ci).next_table;
              Ci := UnZ_IO.Bit_buffer.Read_inverted (E);
            end loop;

            UnZ_IO.Bit_buffer.Dump (Ct (Ci).bits);

            N := Ct (Ci).n;
            if  E /= 0 then
              N := N + UnZ_IO.Bit_buffer.Read_and_dump (8);
            end if;
            S := S - Unsigned_32 (N);

            UnZ_IO.Copy_or_zero (
              distance   => D,
              length     => N,
              index      => W,
              unflushed  => unflushed
            );

          end if;
        end loop;

        UnZ_IO.Flush (W);
        if Zip_EOF then
          raise Zip.Archive_corrupted with "End of stream reached";
        end if;

        if full_trace then
          Ada.Text_IO.Put_Line ("End   Explode_nolit");
        end if;

      end Explode_Nolit;

      procedure Explode (literal_tree, slide_8_KB : Boolean) is

        Tb, Tl, Td : p_Table_list;
        Bb, Bl, Bd : Integer;
        L :  Length_array (0 .. 255);
        huft_incomplete : Boolean;

        cp_length_2_trees :
          constant Length_array (0 .. 63) :=
           (2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17,
           18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
           35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
           52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65);

        cp_length_3_trees :
          constant Length_array (0 .. 63) :=
           (3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
           19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
           36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
           53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66);

        cp_dist_4KB :
          constant Length_array (0 .. 63) :=
          (1, 65, 129, 193, 257, 321, 385, 449, 513, 577, 641, 705,
           769, 833, 897, 961, 1025, 1089, 1153, 1217, 1281, 1345, 1409, 1473,
           1537, 1601, 1665, 1729, 1793, 1857, 1921, 1985, 2049, 2113, 2177,
           2241, 2305, 2369, 2433, 2497, 2561, 2625, 2689, 2753, 2817, 2881,
           2945, 3009, 3073, 3137, 3201, 3265, 3329, 3393, 3457, 3521, 3585,
           3649, 3713, 3777, 3841, 3905, 3969, 4033);

        cp_dist_8KB :
          constant Length_array (0 .. 63) :=
             (1,  129,  257,  385,  513,  641,  769,  897, 1025, 1153, 1281,
           1409, 1537, 1665, 1793, 1921, 2049, 2177, 2305, 2433, 2561, 2689,
           2817, 2945, 3073, 3201, 3329, 3457, 3585, 3713, 3841, 3969, 4097,
           4225, 4353, 4481, 4609, 4737, 4865, 4993, 5121, 5249, 5377, 5505,
           5633, 5761, 5889, 6017, 6145, 6273, 6401, 6529, 6657, 6785, 6913,
           7041, 7169, 7297, 7425, 7553, 7681, 7809, 7937, 8065);

        extra :
          constant Length_array (0 .. 63) := (0 .. 62 => 0, 63 => 8);

      begin
        Bl := 7;
        if UnZ_Glob.compsize > 200000 then
          Bd := 8;
        else
          Bd := 7;
        end if;

        if literal_tree then
          Bb := 9;
          Get_Tree (L);
          begin
            HufT_build (L, 256, empty, empty, Tb, Bb, huft_incomplete);
            if huft_incomplete then
              HufT_free (Tb);
              raise Zip.Archive_corrupted;
            end if;
          exception
            when others =>
              raise Zip.Archive_corrupted;
          end;

          begin
            Get_Tree (L (0 .. 63));
          exception
            when others =>
              HufT_free (Tb);
              raise Zip.Archive_corrupted;
          end;

          begin
            HufT_build (
              L (0 .. 63), 0, cp_length_3_trees, extra, Tl, Bl, huft_incomplete
            );
            if huft_incomplete then
              HufT_free (Tl);
              HufT_free (Tb);
              raise Zip.Archive_corrupted;
            end if;
          exception
            when others =>
              HufT_free (Tb);
              raise Zip.Archive_corrupted;
          end;

          begin
            Get_Tree (L (0 .. 63));
          exception
            when others =>
              HufT_free (Tb);
              HufT_free (Tl);
              raise Zip.Archive_corrupted;
          end;

          begin
            if slide_8_KB then
              HufT_build (
                L (0 .. 63), 0, cp_dist_8KB, extra, Td, Bd, huft_incomplete
              );
              if huft_incomplete then
                HufT_free (Td);
                HufT_free (Tl);
                HufT_free (Tb);
                raise Zip.Archive_corrupted;
              end if;
              --  Exploding, method: 8k slide, 3 trees
              Explode_Lit (7, Tb, Tl, Td, Bb, Bl, Bd);
            else
              HufT_build (
                L (0 .. 63), 0, cp_dist_4KB, extra, Td, Bd, huft_incomplete
              );
              if huft_incomplete then
                HufT_free (Td);
                HufT_free (Tl);
                HufT_free (Tb);
                raise Zip.Archive_corrupted;
              end if;
              --  Exploding, method: 4k slide, 3 trees
              Explode_Lit (6, Tb, Tl, Td, Bb, Bl, Bd);
            end if;
          exception
            when  others =>
              HufT_free (Tl);
              HufT_free (Tb);
              raise Zip.Archive_corrupted;
          end;
          HufT_free (Td);
          HufT_free (Tl);
          HufT_free (Tb);

        else         -- No literal tree

          begin
            Get_Tree (L (0 .. 63));
          exception
            when others =>
              raise Zip.Archive_corrupted;
          end;

          begin
            HufT_build (
              L (0 .. 63), 0, cp_length_2_trees, extra, Tl, Bl, huft_incomplete
            );
            if huft_incomplete then
              HufT_free (Tl);
              raise Zip.Archive_corrupted;
            end if;
          exception
            when others =>
              raise Zip.Archive_corrupted;
          end;

          begin
            Get_Tree (L (0 .. 63));
          exception
            when others =>
              HufT_free (Tl);
              raise Zip.Archive_corrupted;
          end;

          begin
            if slide_8_KB then
              HufT_build (
                L (0 .. 63), 0, cp_dist_8KB, extra, Td, Bd, huft_incomplete
              );
              if huft_incomplete then
                HufT_free (Td);
                HufT_free (Tl);
                raise Zip.Archive_corrupted;
              end if;
              --  Exploding, method: 8k slide, 2 trees
              Explode_Nolit (7, Tl, Td, Bl, Bd);
            else
              HufT_build (
                L (0 .. 63), 0, cp_dist_4KB, extra, Td, Bd, huft_incomplete
              );
              if huft_incomplete then
                HufT_free (Td);
                HufT_free (Tl);
                raise Zip.Archive_corrupted;
              end if;
              --  Exploding, method: 4k slide, 2 trees
              Explode_Nolit (6, Tl, Td, Bl, Bd);
            end if;
          exception
            when others =>
              HufT_free (Tl);
              raise Zip.Archive_corrupted;
          end;
          HufT_free (Td);
          HufT_free (Tl);
        end if;

      end Explode;

      --------[ Method: Copy stored ]--------

      procedure Copy_stored is
        size : constant Zip.Zip_32_Data_Size_Type := UnZ_Glob.compsize;
        read_in, absorbed : Zip.Zip_32_Data_Size_Type;
      begin
        absorbed := 0;
        if Get_mode (local_crypto_pack) = encrypted then
          absorbed := 12;
        end if;
        while absorbed < size loop
          read_in := size - absorbed;
          if read_in > wsize then
            read_in := wsize;
          end if;
          begin
            for I in 0 .. read_in - 1 loop
              UnZ_Glob.slide (Natural (I)) := UnZ_IO.Read_byte_decrypted;
            end loop;
          exception
            when others =>
              raise Zip.Archive_corrupted with
                "End of stream reached (format: Store)";
          end;
          begin
            UnZ_IO.Flush (Natural (read_in));  --  Takes care of CRC too
          exception
            when User_abort =>
              raise;
            when others =>
              raise UnZip.Write_Error;
          end;
          absorbed := absorbed + read_in;
        end loop;
      end Copy_stored;

      --------[ Method: Inflate ]--------

      lt_count,     dl_count,
      lt_count_0,   dl_count_0,
      lt_count_dyn, dl_count_dyn,
      lt_count_fix, dl_count_fix : Long_Integer := 0;  --  Statistics of LZ codes per block

      procedure Inflate_Codes (Tl, Td : p_Table_list; Bl, Bd : Integer) is
        CT      : p_HufT_table;       -- current table
        CT_idx  : Natural;            -- current table's index
        length  : Natural;
        E       : Integer;      -- table entry flag/number of extra bits
        W       : Integer := UnZ_Glob.slide_index;  -- more local variable for slide index
        literal : Zip.Byte;
      begin
        if some_trace then
          lt_count_0 := lt_count;
          dl_count_0 := dl_count;
          Ada.Text_IO.Put_Line ("Begin Inflate_codes");
        end if;

        --  inflate the coded data
        main_loop :
        while not Zip_EOF loop
          if Tl = null then
            raise Zip.Archive_corrupted with
              "Null table list (on data decoding, Huffman tree for literals or LZ lengths)";
          end if;
          CT := Tl.table;
          CT_idx := UnZ_IO.Bit_buffer.Read (Bl);
          loop
            E := CT (CT_idx).extra_bits;
            exit when E <= 16;
            if E = invalid then
              raise Zip.Archive_corrupted;
            end if;

            --  then it's a literal
            UnZ_IO.Bit_buffer.Dump (CT (CT_idx).bits);
            E := E - 16;
            CT := CT (CT_idx).next_table;
            CT_idx := UnZ_IO.Bit_buffer.Read (E);
          end loop;

          UnZ_IO.Bit_buffer.Dump (CT (CT_idx).bits);

          case E is
            when 16 =>      --  CT(CT_idx).N is a Literal (code 0 .. 255)
              literal := Zip.Byte (CT (CT_idx).n);
              if some_trace then
                lt_count := lt_count + 1;
                Ada.Text_IO.Put (LZ77_dump, "Lit" & Zip.Byte'Image (literal));
                if literal in 32 .. 126 then
                  Ada.Text_IO.Put (LZ77_dump, " '" & Character'Val (literal) & ''');
                end if;
                Ada.Text_IO.New_Line (LZ77_dump);
              end if;
              UnZ_Glob.slide (W) :=  literal;
              W := W + 1;
              UnZ_IO.Flush_if_full (W);

            when 15 =>      --  End of block (EOB, code 256)
              if full_trace then
                Ada.Text_IO.Put_Line ("Exit  Inflate_codes, e=15 -> EOB");
              end if;
              exit main_loop;

            when others =>  --  We have a length/distance code
              if some_trace then
                dl_count := dl_count + 1;
              end if;
              --  Get length of block to copy:
              length := CT (CT_idx).n + UnZ_IO.Bit_buffer.Read_and_dump (E);

              --  Decode distance of block to copy:
              if Td = null then
                raise Zip.Archive_corrupted with
                  "Null table list (on data decoding, Huffman tree for LZ distances)";
              end if;
              CT := Td.table;
              CT_idx := UnZ_IO.Bit_buffer.Read (Bd);
              loop
                E := CT (CT_idx).extra_bits;
                exit when E <= 16;
                if E = invalid then
                  raise Zip.Archive_corrupted;
                end if;
                UnZ_IO.Bit_buffer.Dump (CT (CT_idx).bits);
                E := E - 16;
                CT := CT (CT_idx).next_table;
                CT_idx := UnZ_IO.Bit_buffer.Read (E);
              end loop;
              UnZ_IO.Bit_buffer.Dump (CT (CT_idx).bits);
              UnZ_IO.Copy (
                distance    => CT (CT_idx).n + UnZ_IO.Bit_buffer.Read_and_dump (E),
                copy_length => length,
                index       => W
              );
          end case;
        end loop main_loop;

        UnZ_Glob.slide_index := W;

        if some_trace then
          Ada.Text_IO.Put_Line ("End   Inflate_codes;  " &
            Long_Integer'Image (lt_count - lt_count_0) & " literals," &
            Long_Integer'Image (dl_count - dl_count_0) & " DL codes," &
            Long_Integer'Image (dl_count + lt_count - lt_count_0 - dl_count_0) & " in total");
        end if;
      end Inflate_Codes;

      procedure Inflate_stored_block is -- Actually, nothing to inflate
        N : Integer;
      begin
        UnZ_IO.Bit_buffer.Dump_to_byte_boundary;
        --  Get the block length and its complement
        N := UnZ_IO.Bit_buffer.Read_and_dump (16);
        if some_trace then
          Ada.Text_IO.Put_Line ("Begin Inflate_stored_block, bytes stored: " & Integer'Image (N));
        end if;
        if  N /= Integer (
         (not UnZ_IO.Bit_buffer.Read_and_dump_U32 (16))
         and 16#ffff#)
        then
          raise Zip.Archive_corrupted;
        end if;
        while N > 0 and then not Zip_EOF loop
          --  Read and output the non-compressed data
          N := N - 1;
          UnZ_Glob.slide (UnZ_Glob.slide_index) :=
            Zip.Byte (UnZ_IO.Bit_buffer.Read_and_dump (8));
          UnZ_Glob.slide_index := UnZ_Glob.slide_index + 1;
          UnZ_IO.Flush_if_full (UnZ_Glob.slide_index);
        end loop;
        if some_trace then
          Ada.Text_IO.Put_Line ("End   Inflate_stored_block");
        end if;
      end Inflate_stored_block;

      --  Copy lengths for literal codes 257..285

      copy_lengths_literal : Length_array (0 .. 30) :=
             (3,  4,  5,  6,  7,  8,  9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
             35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);

      --  Extra bits for literal codes 257..285

      extra_bits_literal : Length_array (0 .. 30) :=
              (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
               3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, invalid, invalid);

      --  Copy offsets for distance codes 0..29 (30..31: deflate_e)

      copy_offset_distance : constant Length_array (0 .. 31) :=
            (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
             257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
             8193, 12289, 16385, 24577, 32769, 49153);

      --  Extra bits for distance codes

      extra_bits_distance : constant Length_array (0 .. 31) :=
            (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
             7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14);

      max_dist : Integer := 29;  --  changed to 31 for deflate_e

      length_list_for_fixed_block_literals : constant Length_array (0 .. 287) :=
          (0 .. 143 => 8, 144 .. 255 => 9, 256 .. 279 => 7, 280 .. 287 => 8);

      procedure Inflate_fixed_block is
        Tl,                        --   literal/length code table
            Td : p_Table_list;            --  distance code table
        Bl, Bd : Integer;          --  lookup bits for tl/bd
        huft_incomplete : Boolean;
      begin
        if some_trace then
          Ada.Text_IO.Put_Line ("Begin Inflate_fixed_block");
        end if;
        --  Make a complete, but wrong [why ?] code set (see Appnote: 5.5.2, RFC 1951: 3.2.6)
        Bl := 7;
        HufT_build (
          length_list_for_fixed_block_literals, 257, copy_lengths_literal,
          extra_bits_literal, Tl, Bl, huft_incomplete
        );
        --  Make an incomplete code set (see Appnote: 5.5.2, RFC 1951: 3.2.6)
        Bd := 5;
        begin
          HufT_build (
            (0 .. max_dist => 5), 0,
            copy_offset_distance, extra_bits_distance,
            Td, Bd, huft_incomplete
          );
          if huft_incomplete then
            if full_trace then
              Ada.Text_IO.Put_Line (
                "td is incomplete, pointer=null: " &
                Boolean'Image (Td = null)
              );
            end if;
          end if;
        exception
          when huft_out_of_memory | huft_error =>
            HufT_free (Tl);
            raise Zip.Archive_corrupted;
        end;
        --  Decompress the block's data, until an end-of-block code.
        Inflate_Codes (Tl, Td, Bl, Bd);
        --  Done with this block, free resources.
        HufT_free (Tl);
        HufT_free (Td);
        if some_trace then
          Ada.Text_IO.Put_Line ("End   Inflate_fixed_block");
          lt_count_fix := lt_count_fix + (lt_count - lt_count_0);
          dl_count_fix := dl_count_fix + (dl_count - dl_count_0);
        end if;
      end Inflate_fixed_block;

      bit_order_for_dynamic_block : constant array (0 .. 18) of Natural :=
         (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

      procedure Inflate_dynamic_block is

        Lbits : constant := 9;
        Dbits : constant := 6;

        current_length : Natural;
        defined, number_of_lengths : Natural;

        Tl,                             -- literal/length code tables
          Td : p_Table_list;            -- distance code tables

        CT     : p_HufT_table;       -- current table
        CT_idx : Natural;            -- current table's index

        Bl, Bd : Integer;                  -- lookup bits for tl/bd
        Nb : Natural;  -- number of bit length codes
        Nl : Natural;  -- number of literal length codes
        Nd : Natural;  -- number of distance codes

        --  literal/length and distance code lengths
        Ll : Length_array (0 .. 288 + 32 - 1) := (others => 0);

        huft_incomplete : Boolean;

        procedure Repeat_length_code (amount : Natural) is
        begin
          if defined + amount > number_of_lengths then
            raise Zip.Archive_corrupted;
          end if;
          for c in reverse 1 .. amount loop
            Ll (defined) := current_length;
            defined := defined + 1;
          end loop;
        end Repeat_length_code;

      begin
        if some_trace then
          Ada.Text_IO.Put_Line ("Begin Inflate_dynamic_block");
        end if;

        --  Read in table lengths
        Nl := 257 + UnZ_IO.Bit_buffer.Read_and_dump (5);
        Nd :=   1 + UnZ_IO.Bit_buffer.Read_and_dump (5);
        Nb :=   4 + UnZ_IO.Bit_buffer.Read_and_dump (4);

        if Nl > 288 or else Nd > 32 then
          raise Zip.Archive_corrupted;
        end if;

        --  Read in bit-length-code lengths for decoding the compression structure.
        --  The rest, Ll( Bit_Order( Nb .. 18 ) ), is already = 0
        for J in  0 .. Nb - 1  loop
          Ll (bit_order_for_dynamic_block (J)) := UnZ_IO.Bit_buffer.Read_and_dump (3);
        end loop;

        --  Build decoding table for trees--single level, 7 bit lookup
        Bl := 7;
        begin
          HufT_build (
            Ll (0 .. 18), 19, empty, empty, Tl, Bl, huft_incomplete
          );
          if huft_incomplete then
            HufT_free (Tl);
            raise Zip.Archive_corrupted with "Incomplete code set for compression structure";
          end if;
        exception
          when others =>
            raise Zip.Archive_corrupted with "Error when building tables for compression structure";
        end;

        --  Read in the compression structure: literal and distance code lengths
        number_of_lengths := Nl + Nd;
        defined := 0;
        current_length := 0;

        while  defined < number_of_lengths  loop
          if Tl = null then
            raise Zip.Archive_corrupted with
            "Null table list (on compression structure)";
          end if;
          CT := Tl.table;
          CT_idx := UnZ_IO.Bit_buffer.Read (Bl);
          UnZ_IO.Bit_buffer.Dump (CT (CT_idx).bits);

          case CT (CT_idx).n is
            when 0 .. 15 =>     --  Length of code for symbol of index 'defined', in bits (0..15)
              current_length := CT (CT_idx).n;
              Ll (defined) := current_length;
              defined := defined + 1;
            when 16 =>          --  16 means: repeat last bit length 3 to 6 times
              if defined = 0 then
                --  Nothing in the Ll array has been defined so far. Then, current_length is
                --  (theoretically) undefined and cannot be repeated.
                --  This unspecified case is treated as an error by zlib's inflate.c.
                raise Zip.Archive_corrupted with
                  "Illegal data for compression structure (repeat an undefined code length)";
              end if;
              Repeat_length_code (3 + UnZ_IO.Bit_buffer.Read_and_dump (2));
            when 17 =>          --  17 means: the next 3 to 10 symbols' codes have zero bit lengths
              current_length := 0;
              Repeat_length_code (3 + UnZ_IO.Bit_buffer.Read_and_dump (3));
            when 18 =>          --  18 means: the next 11 to 138 symbols' codes have zero bit lengths
              current_length := 0;
              Repeat_length_code (11 + UnZ_IO.Bit_buffer.Read_and_dump (7));
            when others =>      --  Shouldn't occur if this tree is correct
              raise Zip.Archive_corrupted with
                "Illegal data for compression structure (values should be in the range 0 .. 18): "
                & Integer'Image (CT (CT_idx).n);
          end case;
        end loop;
        --  Free the Huffman tree that was used for decoding the compression
        --  structure, which is contained now in Ll.
        HufT_free (Tl);
        if Ll (256) = 0 then
          --  No code length for the End-Of-Block symbol, implies infinite stream!
          --  This case is unspecified but obviously we must stop here.
          raise Zip.Archive_corrupted with "No code length for End-Of-Block symbol #256";
        end if;
        --  Build the decoding tables for literal/length codes
        Bl := Lbits;
        begin
          HufT_build (
            Ll (0 .. Nl - 1), 257,
            copy_lengths_literal, extra_bits_literal,
            Tl, Bl, huft_incomplete
          );
          if huft_incomplete then
            HufT_free (Tl);
            raise Zip.Archive_corrupted with "Incomplete code set for literals/lengths";
          end if;
        exception
          when others =>
            raise Zip.Archive_corrupted with "Error when building tables for literals/lengths";
        end;
        --  Build the decoding tables for distance codes
        Bd := Dbits;
        begin
          HufT_build (
            Ll (Nl .. Nl + Nd - 1), 0,
            copy_offset_distance, extra_bits_distance,
            Td, Bd, huft_incomplete
          );
          if huft_incomplete then
            if deflate_strict then
              raise Zip.Archive_corrupted with "Incomplete code set for distances";
            elsif some_trace then  --  not deflate_strict => don't stop
              Ada.Text_IO.Put_Line ("Huffman tree incomplete - PKZIP 1.93a bug workaround");
            end if;
          end if;
        exception
          when huft_out_of_memory | huft_error =>
            HufT_free (Tl);
            raise Zip.Archive_corrupted with "Error when building tables for distances";
        end;
        --  Decompress the block's data, until an end-of-block code.
        Inflate_Codes (Tl, Td, Bl, Bd);
        --  Done with this block, free resources.
        HufT_free (Tl);
        HufT_free (Td);
        if some_trace then
          Ada.Text_IO.Put_Line ("End   Inflate_dynamic_block");
          lt_count_dyn := lt_count_dyn + (lt_count - lt_count_0);
          dl_count_dyn := dl_count_dyn + (dl_count - dl_count_0);
        end if;
      end Inflate_dynamic_block;

      procedure Inflate_Block (last_block : out Boolean; fix, dyn : in out Long_Integer) is
      begin
        last_block := Boolean'Val (UnZ_IO.Bit_buffer.Read_and_dump (1));
        case UnZ_IO.Bit_buffer.Read_and_dump (2) is  --  Block type = 0, 1, 2, 3
          when 0 =>      Inflate_stored_block;
          when 1 =>      Inflate_fixed_block;
                         fix := fix + 1;
          when 2 =>      Inflate_dynamic_block;
                         dyn := dyn + 1;
          when others => raise Zip.Archive_corrupted with "Inflate: Bad block type (3)";
        end case;
      end Inflate_Block;

      procedure Inflate is
        is_last_block : Boolean;
        blocks, blocks_fix, blocks_dyn : Long_Integer := 0;
      begin
        if deflate_e_mode then
          copy_lengths_literal (28) := 3;  --  instead of 258
          extra_bits_literal (28) := 16;   --  instead of 0
          max_dist := 31;
        end if;
        loop
          blocks := blocks + 1;
          Inflate_Block (is_last_block, blocks_fix, blocks_dyn);
          exit when is_last_block;
        end loop;
        UnZ_IO.Flush (UnZ_Glob.slide_index);
        UnZ_Glob.slide_index := 0;
        if some_trace then
          Ada.Text_IO.Put_Line (
            "# blocks:" & Long_Integer'Image (blocks) &
            "; fixed:" & Long_Integer'Image (blocks_fix) &
            "; dynamic:" & Long_Integer'Image (blocks_dyn));
          if blocks_fix > 0 then
            Ada.Text_IO.Put_Line (
              "Averages per fixed block: literals:" & Long_Integer'Image (lt_count_fix / blocks_fix) &
              "; DL codes:" & Long_Integer'Image (dl_count_fix / blocks_fix) &
              "; all codes:" & Long_Integer'Image ((lt_count_fix + dl_count_fix) / blocks_fix));
          end if;
          if blocks_dyn > 0 then
            Ada.Text_IO.Put_Line (
              "Averages per dynamic block: literals:" & Long_Integer'Image (lt_count_dyn / blocks_dyn) &
              "; DL codes:" & Long_Integer'Image (dl_count_dyn / blocks_dyn) &
              "; all codes:" & Long_Integer'Image ((lt_count_dyn + dl_count_dyn) / blocks_dyn));
          end if;
        end if;
      end Inflate;

      --------[ Method: BZip2 ]--------

      procedure Bunzip2 is
        type BZ_Buffer is array (Natural range <>) of Interfaces.Unsigned_8;
        procedure UD_Read (b : out BZ_Buffer) is
        pragma Inline (UD_Read);
        begin
          for i in b'Range loop
            b (i) := UnZ_IO.Read_byte_decrypted;
          end loop;
        end UD_Read;
        procedure UD_Write (b : in BZ_Buffer) is
        pragma Inline (UD_Write);
        begin
          for i in b'Range loop
            UnZ_Glob.slide (UnZ_Glob.slide_index) := b (i);
            UnZ_Glob.slide_index := UnZ_Glob.slide_index + 1;
            UnZ_IO.Flush_if_full (UnZ_Glob.slide_index);
          end loop;
        end UD_Write;
        package My_BZip2 is new BZip2.Decoding
           (Buffer    => BZ_Buffer,
            check_CRC => False,  --  CRC check is already done by UnZ_IO
            Read      => UD_Read,
            Write     => UD_Write
          );
      begin
        My_BZip2.Decompress;
        UnZ_IO.Flush (UnZ_Glob.slide_index);
      exception
        when E : My_BZip2.bad_header_magic | My_BZip2.bad_block_magic | My_BZip2.data_error =>
          raise Zip.Archive_corrupted with
            "BZip2 error: " & Exception_Name (E) & " - " & Exception_Message (E);
        when E : My_BZip2.randomized_not_yet_implemented =>
          raise UnZip.Unsupported_method with
            "BZip2: " & Exception_Name (E) & " - " & Exception_Message (E);
      end Bunzip2;

      --------[ Method: LZMA ]--------

      procedure LZMA_Decode is
        --
        procedure Write_Single_Byte (b : Unsigned_8) is
        pragma Inline (Write_Single_Byte);
        begin
          UnZ_Glob.slide (UnZ_Glob.slide_index) := b;
          UnZ_Glob.slide_index := UnZ_Glob.slide_index + 1;
          UnZ_IO.Flush_if_full (UnZ_Glob.slide_index);
        end Write_Single_Byte;
        --
        package My_LZMA_Decoding is new LZMA.Decoding (UnZ_IO.Read_byte_decrypted, Write_Single_Byte);
        b3, b4 : Unsigned_8;
      begin
        b3 := UnZ_IO.Read_byte_decrypted;  --  LZMA SDK major version (e.g.: 9)
        b3 := UnZ_IO.Read_byte_decrypted;  --  LZMA SDK minor version (e.g.: 20)
        b3 := UnZ_IO.Read_byte_decrypted;  --  LZMA properties size low byte
        b4 := UnZ_IO.Read_byte_decrypted;  --  LZMA properties size high byte
        if Natural (b3) + 256 * Natural (b4) /= 5 then
          raise Zip.Archive_corrupted with "Unexpected LZMA properties block size";
        end if;
        My_LZMA_Decoding.Decompress (
          (has_size               => False,  --  Data size is not part of the LZMA header.
           given_size             => LZMA.Data_Bytes_Count (UnZ_Glob.uncompsize),
           marker_expected        => explode_slide_8KB_LZMA_EOS,  --  End-Of-Stream marker?
           fail_on_bad_range_code => True)
        );
        UnZ_IO.Flush (UnZ_Glob.slide_index);
      exception
        when E : My_LZMA_Decoding.LZMA_Error =>
          raise Zip.Archive_corrupted with
            "LZMA error: " & Exception_Name (E) & " - " & Exception_Message (E);
      end LZMA_Decode;

    end UnZ_Meth;

    procedure Process_descriptor (dd : out Zip.Headers.Data_descriptor) is
      start : Integer;
      b : Unsigned_8;
      dd_buffer : Zip.Byte_Buffer (1 .. 30);
    begin
      UnZ_IO.Bit_buffer.Dump_to_byte_boundary;
      Set_mode (local_crypto_pack, clear); -- We are after compressed data, switch off decryption.
      b := UnZ_IO.Read_byte_decrypted;
      if b = 75 then -- 'K' ('P' is before, this is a Java/JAR bug!)
        dd_buffer (1) := 80;
        dd_buffer (2) := 75;
        start := 3;
      else
        dd_buffer (1) := b; -- hopefully = 80 (will be checked)
        start := 2;
      end if;
      for i in start .. 16 loop
        dd_buffer (i) := UnZ_IO.Read_byte_decrypted;
      end loop;
      Zip.Headers.Copy_and_check (dd_buffer, dd);
    exception
      when Zip.Headers.bad_data_descriptor =>
        raise Zip.Archive_corrupted;
    end Process_descriptor;

    work_index : Zip_Streams.ZS_Index_Type;
    use Zip, UnZ_Meth, Ada.Strings.Unbounded;

  begin  --  Decompress_Data
    if some_trace then
      Ada.Text_IO.Create (LZ77_dump, Ada.Text_IO.Out_File, "dump.lz77");
    end if;
    output_memory_access := null;
    --  ^ this is an 'out' parameter, we have to set it anyway
    case mode is
      when write_to_binary_file =>
         Ada.Streams.Stream_IO.Create (UnZ_IO.out_bin_file, Ada.Streams.Stream_IO.Out_File, output_file_name,
                                         Form => To_String (Zip_Streams.Form_For_IO_Open_and_Create));
      when write_to_text_file =>
         Ada.Text_IO.Create (UnZ_IO.out_txt_file, Ada.Text_IO.Out_File, output_file_name,
                               Form => To_String (Zip_Streams.Form_For_IO_Open_and_Create));
      when write_to_memory =>
        output_memory_access := new
          Ada.Streams.Stream_Element_Array (
            1 .. Ada.Streams.Stream_Element_Offset (hint.dd.uncompressed_size)
          );
        UnZ_Glob.uncompressed_index := output_memory_access'First;
      when write_to_stream | just_test =>
        null;
    end case;

    UnZ_Glob.compsize  := hint.dd.compressed_size;
    if UnZ_Glob.compsize > Zip_32_Data_Size_Type'Last - 2 then  --  This means: unknown size
      UnZ_Glob.compsize := Zip_32_Data_Size_Type'Last - 2;      --  Avoid wraparound in read_buffer
    end if;                                              --  From TT's version, 2008
    UnZ_Glob.uncompsize := hint.dd.uncompressed_size;
    UnZ_IO.Init_Buffers;
    if is_encrypted then
      Set_mode (local_crypto_pack, encrypted);
      work_index := Zip_Streams.Index (zip_file);
      password_passes : for pass in 1 .. tolerance_wrong_password loop
        begin
          Init_Decryption (To_String (password), hint.dd.crc_32);
          exit password_passes; -- the current password fits, then go on!
        exception
          when Wrong_password =>
            if pass = tolerance_wrong_password then
              raise;
            elsif get_new_password /= null then
              get_new_password (password);  --  ask for a new one
            end if;
        end;
        --  Go back to data beginning:
        begin
          Zip_Streams.Set_Index (zip_file, work_index);
        exception
          when others =>
            raise UnZip.Read_Error with "Failure after password interaction";
        end;
        UnZ_IO.Init_Buffers;
      end loop password_passes;
    else
      Set_mode (local_crypto_pack, clear);
    end if;

    --  UnZip correct type
    begin
      case format is
        when store          => Copy_stored;
        when shrink         => Unshrink;
        when Reduce_Format  => Unreduce (1 + Reduce_Format'Pos (format) - Reduce_Format'Pos (reduce_1));
        when implode        =>
          UnZ_Meth.Explode (explode_literal_tree, explode_slide_8KB_LZMA_EOS);
        when deflate | deflate_e =>
          UnZ_Meth.deflate_e_mode := format = deflate_e;
          UnZ_Meth.Inflate;
        when Zip.bzip2      => UnZ_Meth.Bunzip2;
        when Zip.lzma_meth  => UnZ_Meth.LZMA_Decode;
        when others =>
          raise Unsupported_method with
            "Format/method " & Image (format) &
            " not supported for decompression";
      end case;
    exception
      when others =>
        UnZ_IO.Delete_output;
        raise;
    end;
    UnZ_Glob.crc32val := Zip.CRC_Crypto.Final (UnZ_Glob.crc32val);
    --  Decompression done !

    if data_descriptor_after_data then  --  Sizes and CRC at the end
      declare
        memo_uncomp_size : constant Unsigned_32 := hint.dd.uncompressed_size;
      begin
        Process_descriptor (hint.dd);  --  CRC is for checking; sizes are for informing user
        if memo_uncomp_size < Unsigned_32'Last and then --
           memo_uncomp_size /= hint.dd.uncompressed_size
        then
          UnZ_IO.Delete_output;
          raise Uncompressed_size_Error;
        end if;
      end;
    end if;

    if hint.dd.crc_32 /= UnZ_Glob.crc32val then
      UnZ_IO.Delete_output;
      raise CRC_Error with
        "CRC stored in archive: " & Hexadecimal (hint.dd.crc_32) &
        "; CRC computed now: " & Hexadecimal (UnZ_Glob.crc32val);
    end if;

    case mode is
      when write_to_binary_file =>
        Ada.Streams.Stream_IO.Close (UnZ_IO.out_bin_file);
      when write_to_text_file =>
        Ada.Text_IO.Close (UnZ_IO.out_txt_file);
      when write_to_memory | write_to_stream | just_test =>
        null;  --  Nothing to close!
    end case;
    if some_trace then
      Ada.Text_IO.Close (LZ77_dump);
    end if;

  exception
    when others =>  --  close the file in case of an error, if not yet closed
      case mode is  --  or deleted
        when write_to_binary_file =>
          if Ada.Streams.Stream_IO.Is_Open (UnZ_IO.out_bin_file) then
            Ada.Streams.Stream_IO.Close (UnZ_IO.out_bin_file);
          end if;
        when write_to_text_file =>
          if Ada.Text_IO.Is_Open (UnZ_IO.out_txt_file) then
            Ada.Text_IO.Close (UnZ_IO.out_txt_file);
          end if;
        when write_to_memory | write_to_stream | just_test =>
          null;  --  Nothing to close!
      end case;
      raise;
  end Decompress_data;

end UnZip.Decompress;
