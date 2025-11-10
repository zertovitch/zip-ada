--  Legal licensing note:

--  Copyright (c) 2006 .. 2025 Gautier de Montmollin (see spec. for credits)
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

with Ada.Unchecked_Deallocation;
with Shrink;

procedure Zip.Compress.Shrink_E
  (input,
   output           : in out Zip_Streams.Root_Zipstream_Type'Class;
   input_size_known :        Boolean;
   input_size       :        Zip_64_Data_Size_Type;  --  ignored if unknown
   feedback         :        Feedback_Proc;
   CRC              : in out Interfaces.Unsigned_32;  --  only updated here
   crypto           : in out CRC_Crypto.Crypto_pack;
   output_size      :    out Zip_64_Data_Size_Type;
   compression_ok   :    out Boolean)  --  indicates compressed < uncompressed
is
  use Interfaces;

  ------------------
  -- Buffered I/O --
  ------------------

  IO_buffers : IO_Buffers_Type;

  procedure Put_Byte (B : Unsigned_8) is
  begin
    IO_buffers.OutBuf (IO_buffers.OutBufIdx) := B;
    IO_buffers.OutBufIdx := IO_buffers.OutBufIdx + 1;
    if IO_buffers.OutBufIdx > IO_buffers.OutBuf.all'Last then
      Write_Block (IO_buffers, input_size_known, input_size, output, output_size, crypto);
    end if;
  end Put_Byte;

  procedure Flush_Output is
  begin
    if IO_buffers.OutBufIdx > 1 then
      Write_Block (IO_buffers, input_size_known, input_size, output, output_size, crypto);
    end if;
  end Flush_Output;

  --------------------------------------------------------------------------

  ------------------------------------------------------
  --  Bit code buffer, for sending data at bit level  --
  ------------------------------------------------------

  --  Output buffer. Bits are inserted starting at the right (least
  --  significant bits). The width of bit_buffer must be at least 16 bits.
  subtype U32 is Unsigned_32;
  bit_buffer : U32 := 0;
  --  Number of valid bits in bit_buffer.  All bits above the last valid bit are always zero.
  valid_bits : Integer := 0;

  procedure Flush_Bit_Buffer is
  begin
    while valid_bits > 0 loop
      Put_Byte (Byte (bit_buffer and 16#FF#));
      bit_buffer := Shift_Right (bit_buffer, 8);
      valid_bits := Integer'Max (0, valid_bits - 8);
    end loop;
    bit_buffer := 0;
  end Flush_Bit_Buffer;

  subtype Code_Size_Type is Integer range 1 .. Shrink.Maximum_Code_Bit_Size;
  code_size : Code_Size_Type;     --  Size of codes (in bits) currently being written

  --  Send a value on a given number of bits.
  procedure Put_Code (code : Natural) is
  pragma Inline (Put_Code);
  begin
    --  Put bits from code at the left of existing ones. They might be shifted away
    --  partially on the left side (or even entirely if valid_bits is already = 32).
    bit_buffer := bit_buffer or Shift_Left (U32 (code), valid_bits);
    valid_bits := valid_bits + code_size;
    if valid_bits > 32 then
      --  Flush 32 bits to output as 4 bytes
      Put_Byte (Byte (bit_buffer and 16#FF#));
      Put_Byte (Byte (Shift_Right (bit_buffer,  8) and 16#FF#));
      Put_Byte (Byte (Shift_Right (bit_buffer, 16) and 16#FF#));
      Put_Byte (Byte (Shift_Right (bit_buffer, 24) and 16#FF#));
      valid_bits := valid_bits - 32;
      --  Empty buffer and put on it the rest of the code
      bit_buffer := Shift_Right (U32 (code), code_size - valid_bits);
    end if;
  end Put_Code;

  is_table_full : Boolean;  --  Flag indicating a full symbol table

  --  Define data types needed to implement a code table for LZW compression
  type Code_Rec is record
    child   : Integer;       --  Index of 1st suffix for this prefix
    sibling : Integer;       --  Index of next suffix in chain
    suffix  : Natural;       --  Suffix
  end record;

  Table_Last : constant := 2 ** Shrink.Maximum_Code_Bit_Size - 1;

  type Code_Array is array (0 .. Table_Last) of Code_Rec;
  --  Define the code table

  type Table_Access is access Code_Array;
  procedure Dispose is new Ada.Unchecked_Deallocation (Code_Array, Table_Access);

  Code_table : Table_Access := null;  --  Points to code table for LZW compression

  --  Define data types needed to implement a free node list
  type Free_list_array is array (Shrink.First_Entry .. Table_Last) of Natural;
  type Free_list_access is access Free_list_array;

  procedure Dispose is
    new Ada.Unchecked_Deallocation (Free_list_array, Free_list_access);

  Free_list : Free_list_access := null;  --  Table of free code table entries
  Next_free : Integer;                   --  Index into free list table

  ----------------------------------------------------------------------------
  --  The following routines are used to allocate, initialize, and de-allocate
  --  various dynamic memory structures used by the LZW compression algorithm
  ----------------------------------------------------------------------------

  procedure Build_Data_Structures is
  begin
    Code_table := new Code_Array;
    Free_list  := new Free_list_array;
  end Build_Data_Structures;

  ---------------------------------------------------------------------------
  procedure Destroy_Data_Structures is
  begin
    Dispose (Code_table);
    Dispose (Free_list);
  end Destroy_Data_Structures;

  ---------------------------------------------------------------------------

  procedure Initialize_Data_Structures is
  begin
    for I in 0 .. Table_Last loop
      Code_table (I).child   := Shrink.Unused;
      Code_table (I).sibling := Shrink.Unused;
      if I <= 255 then
        Code_table (I).suffix := I;
      end if;
      if I >= 257 then
        Free_list (I) := I;
      end if;
    end loop;
    Next_free := Shrink.First_Entry;
    is_table_full := False;
  end Initialize_Data_Structures;

  ---------------------------------------------------------------------------
  --  The following routines handle manipulation of the LZW Code Table
  ---------------------------------------------------------------------------

  clear_list : array (0 .. 1023) of Unsigned_8;
  --  Bit mapped structure used in during adaptive resets

  procedure Prune (Parent : Integer) is
    --  Prune leaves from a subtree - Note: this is a recursive procedure
    current_child : Integer;
    next_sibling : Integer;
  begin
    current_child := Code_table (Parent).child;
    --  Find first Child that has descendants .. clear any that don't

    while current_child /= Shrink.Unused and then
          Code_table (current_child).child = Shrink.Unused
    loop
      Code_table (Parent).child := Code_table (current_child).sibling;
      Code_table (current_child).sibling := Shrink.Unused;
      --  Turn on clear_list bit to indicate a cleared entry
      clear_list (current_child / 8) :=
          clear_list (current_child / 8)  or
          (Shift_Left (1, current_child  mod  8));
      current_child := Code_table (Parent).child;
    end loop;

    if current_child /= Shrink.Unused then    --  If there are any children left ...
      Prune (current_child);
      next_sibling := Code_table (current_child).sibling;
      while next_sibling /= Shrink.Unused loop
        if  Code_table (next_sibling).child = Shrink.Unused then
          Code_table (current_child).sibling :=
            Code_table (next_sibling).sibling;
          Code_table (next_sibling).sibling := Shrink.Unused;
          --  Turn on clear_list bit to indicate a cleared entry

          clear_list (next_sibling / 8) :=
            clear_list (next_sibling / 8)  or
            (Shift_Left (1, next_sibling  mod  8));
          next_sibling := Code_table (current_child).sibling;
        else
          current_child := next_sibling;
          Prune (current_child);
          next_sibling := Code_table (current_child).sibling;
        end if;
      end loop;
    end if;
  end Prune;

  ---------------------------------------------------------------------------

  procedure Clear_Table is
  begin
    clear_list := (others => 0);
    --  Remove all leaf nodes by recursively pruning subtrees
    for Node in  0 .. 255 loop
      Prune (Node);
    end loop;
    --  Next, re-initialize our list of free table entries
    Next_free := Table_Last + 1;
    for Node in reverse Shrink.First_Entry .. Table_Last loop
      if (clear_list (Node / 8)  and  (Shift_Left (1, Node  mod  8))) /= 0 then
        Next_free := Next_free - 1;
        Free_list (Next_free) := Node;
      end if;
    end loop;
    --
    is_table_full := Next_free > Table_Last;
  end Clear_Table;

  ---------------------------------------------------------------------------

  procedure Table_Add (prefix_initially : Natural; suffix : Natural) is
    free_node : Natural;
    prefix : Natural := prefix_initially;
  begin
    if Next_free <= Table_Last then
      free_node := Free_list (Next_free);
      Next_free := Next_free + 1;
      Code_table (free_node).child := Shrink.Unused;
      Code_table (free_node).sibling := Shrink.Unused;
      Code_table (free_node).suffix := suffix;
      if Code_table (prefix).child = Shrink.Unused then
        Code_table (prefix).child := free_node;
      else
        prefix := Code_table (prefix).child;
        while Code_table (prefix).sibling /= Shrink.Unused loop
          prefix := Code_table (prefix).sibling;
        end loop;
        Code_table (prefix).sibling := free_node;
      end if;
    end if;
    --
    is_table_full := Next_free > Table_Last;
  end Table_Add;

  ---------------------------------------------------------------------------

  ---------------------------------------------------------------------------
  --  Search for a Prefix:Suffix pair in our Symbol table. If found, return
  --  the index value where found.  If not found, return False and set
  --  Found_at to UNUSED.
  ---------------------------------------------------------------------------
  procedure Table_Lookup
    (target_prefix : in     Integer;
     target_suffix : in     Natural;
     found_at      :    out Integer;
     found         :    out Boolean)
  is
    --  Was in 16-bit ASM
    idx : Natural := target_prefix;
  begin
    --  Lookup an entry in the Hash Table. If found, return TRUE and set
    --  parameter Found_at with the index of the entry at which the match
    --  was found. If not found, return False and plug an UNUSED into Found_at.
    if Code_table (idx).child = Shrink.Unused then
      found_at := Shrink.Unused;
      found := False;
    else
      idx := Code_table (idx).child;
      loop
        if Code_table (idx).suffix = target_suffix then
          found_at := idx;
          found := True;
          return;
        elsif Code_table (idx).sibling = Shrink.Unused then
          found_at := Shrink.Unused;
          found := False;
          return;
        else
          idx := Code_table (idx).sibling;
        end if;
      end loop;
    end if;
  end Table_Lookup;

  ---------------------------------------------------------------------------
  --  The actual Crunching algorithm
  ---------------------------------------------------------------------------

  Last_code : Integer := 0;
  is_first_atom : Boolean;     --  Flag indicating the START of a shrink operation
  current_max_code : Natural;  --  Largest code that can be written in code_size bits

  procedure Shrink_Atom (Suffix : Integer) is
    where_found  : Integer;
    is_lookup_ok : Boolean;
  begin
    if is_first_atom then            --  If just getting started ...
      bit_buffer := 0;
      valid_bits := 0;
      code_size  := Shrink.Minimum_Code_Bit_Size;
      current_max_code   := 2 ** code_size - 1;
      Last_code  := Suffix;      --    get first character from input,
      is_first_atom := False;    --    and reset the first char flag.
    elsif Suffix = Shrink.Unused then
      --  Nothing to crunch... must be EOF on input
      Put_Code (Last_code);         --  Write last prefix code
      Flush_Bit_Buffer;
      Flush_Output;
    elsif is_table_full then
      Put_Code (Last_code);
      --  NB: PKZip does not necessarily clear the table when
      --  it is full. Hence the need for the special code below.
      Put_Code (Shrink.Special_Code);
      Put_Code (Shrink.Code_for_clearing_table);
      Clear_Table;
      Table_Add (Last_code, Suffix);
      Last_code := Suffix;
    else
      Table_Lookup (Last_code, Suffix, where_found, is_lookup_ok);
      if is_lookup_ok then
        --  If Last_code:Suffix pair is found in the code table, then ...
        --  ... set Last_code to the entry where the pair is located
        Last_code := where_found;
      else
        --  Not in table
        Put_Code (Last_code);           --  Write current Last_code code
        Table_Add (Last_code, Suffix);  --  Attempt to add to code table
        Last_code := Suffix;            --  Reset Last_code code for new char

        if (code_size < Shrink.Maximum_Code_Bit_Size and not is_table_full)
            --  12-Dec-2007: the Pascal code had an out-of-range access
            --    with Free_list(Next_free) below when the table was full!
            --    NB: according to tests, and surely it can be proven,
            --    the case (Code_size < Max_bits and Table_Full) never happens,
            --    so that
            --      "Code_size < Max_bits and then Free_list(Next_free) > Max_code"
            --    could be sufficient. But until it is proven, I prefer to
            --    keep the "and not Table_Full"
          and then
            Free_list (Next_free) > current_max_code
        then
          --  Time to increase the code size and change the max. code
          Put_Code (Shrink.Special_Code);
          Put_Code (Shrink.Code_for_increasing_code_size);
          code_size := code_size + 1;
          current_max_code := 2 **  code_size - 1;
        end if;
      end if;
    end if;
  end Shrink_Atom;

  feedback_milestone,
  Bytes_in   : Zip_Streams.ZS_Size_Type := 0;   --  Count of input file bytes processed

  procedure Process_Input (Source : Byte_Buffer) is
    PctDone : Natural;
    user_aborting : Boolean;
    Last_processed : Integer := Source'First - 1;
    use Zip_Streams;
  begin
    if Source'Length < 1 then
      Shrink_Atom (Shrink.Unused);
    else
      for I in Source'Range loop
        Bytes_in := Bytes_in + 1;
        if feedback /= null then
          if Bytes_in = 1 then
            feedback (0, False, user_aborting);
          end if;
          if feedback_milestone > 0 and then --  Bugfix GdM 23-Dec-2002
             ((Bytes_in - 1) mod feedback_milestone = 0
              or Bytes_in = ZS_Size_Type (input_size))
          then
            if input_size_known then
              PctDone := Integer ((100.0 * Float (Bytes_in)) / Float (input_size));
              feedback (PctDone, False, user_aborting);
            else
              feedback (0, False, user_aborting);
            end if;
            if user_aborting then
              raise User_abort;
            end if;
          end if;
        end if;
        Shrink_Atom (Integer (Source (I)));
        Last_processed := I;
        if input_size_known and then Bytes_in >= ZS_Size_Type (input_size) then
          --  The job is done, even though there are more in the buffer
          IO_buffers.InputEoF := True;
          exit;
        end if;
      end loop;
      Zip.CRC_Crypto.Update (CRC, Source (Source'First .. Last_processed));
    end if;
  end Process_Input;

  procedure Deallocation is
  begin
    Destroy_Data_Structures;
    Deallocate_Buffers (IO_buffers);
  end Deallocation;

  Remaining : Natural;

begin
  Allocate_Buffers (IO_buffers, input_size_known, input_size);
  Build_Data_Structures;
  Initialize_Data_Structures;
  output_size := 0;
  --
  begin
    Read_Block (IO_buffers, input);                --  Prime the input buffer
    is_first_atom   := True;         --  1st character flag for Crunch procedure
    if input_size_known then
      feedback_milestone := Zip_Streams.ZS_Size_Type (input_size / feedback_steps);
    end if;
    while not IO_buffers.InputEoF loop
      Remaining := IO_buffers.MaxInBufIdx - IO_buffers.InBufIdx + 1;
      if Remaining = 0 then
        Read_Block (IO_buffers, input);
      else
        Process_Input (IO_buffers.InBuf (IO_buffers.InBufIdx .. IO_buffers.InBufIdx + Remaining - 1));
        IO_buffers.InBufIdx := IO_buffers.InBufIdx + Remaining;
      end if;
    end loop;
    Process_Input (IO_buffers.InBuf (1 .. 0));  --  This forces EOF processing
    compression_ok := Bytes_in > 0;
  exception
    when Compression_inefficient =>
      compression_ok := False;
  end;
  --
  Deallocation;
exception
  when others =>
    Deallocation;
    raise;
end Zip.Compress.Shrink_E;
