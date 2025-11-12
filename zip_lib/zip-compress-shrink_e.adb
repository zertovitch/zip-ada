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

  subtype Code_Size_Type is Integer range 1 .. Shrink.maximum_code_bit_size;
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

  --  Define data types needed to implement a code table for LZW compression
  type Code_Rec is record
    child   : Integer;  --  Index of 1st suffix for this prefix
    sibling : Integer;  --  Index of next suffix in chain
    suffix  : Natural;
  end record;

  table_last : constant := 2 ** Shrink.maximum_code_bit_size - 1;

  type Code_Table_Array is array (0 .. table_last) of Code_Rec;

  type Code_Table_Access is access Code_Table_Array;

  procedure Dispose is new Ada.Unchecked_Deallocation (Code_Table_Array, Code_Table_Access);

  table : Code_Table_Access := new Code_Table_Array;

  --  Define data types needed to implement a free node list
  type Free_List_Array is array (Shrink.first_entry .. table_last) of Natural;
  type Free_List_Access is access Free_List_Array;

  procedure Dispose is new Ada.Unchecked_Deallocation (Free_List_Array, Free_List_Access);

  free_list : Free_List_Access := new Free_List_Array;  --  Table of free code table entries
  next_free : Integer := Shrink.first_entry;            --  Index into free list table

  function Is_Table_Full return Boolean is (next_free > table_last) with Inline;

  procedure Initialize_Data_Structures is
  begin
    for i in 0 .. table_last loop
      table (i).child   := Shrink.unused;
      table (i).sibling := Shrink.unused;
      if i <= 255 then
        table (i).suffix := i;
      elsif i >= 257 then
        free_list (i) := i;
      end if;
    end loop;
  end Initialize_Data_Structures;

  ---------------------------------------------------------------------------
  procedure Destroy_Data_Structures is
  begin
    Dispose (table);
    Dispose (free_list);
  end Destroy_Data_Structures;

  ---------------------------------------------------------------------------
  --  The following routines handle manipulation of the LZW Code Table
  ---------------------------------------------------------------------------

  clear_list : array (0 .. table_last) of Boolean;

  procedure Prune (parent : Integer) is
    --  Prune leaves from a subtree.
    --  Note:  - this is a recursive procedure
    --         - !! can be simplified !!
    current_child : Integer;
    next_sibling : Integer;
  begin
    current_child := table (parent).child;
    --  Find first Child that has descendants .. clear any that don't

    while current_child /= Shrink.unused and then
          table (current_child).child = Shrink.unused
    loop
      table (parent).child := table (current_child).sibling;
      table (current_child).sibling := Shrink.unused;
      clear_list (current_child) := True;
      current_child := table (parent).child;
    end loop;

    if current_child /= Shrink.unused then    --  If there are any children left ...
      Prune (current_child);
      next_sibling := table (current_child).sibling;

      while next_sibling /= Shrink.unused loop
        if table (next_sibling).child = Shrink.unused then
          table (current_child).sibling := table (next_sibling).sibling;
          table (next_sibling).sibling := Shrink.unused;
          clear_list (next_sibling) := True;
          next_sibling := table (current_child).sibling;
        else
          current_child := next_sibling;
          Prune (current_child);
          next_sibling := table (current_child).sibling;
        end if;
      end loop;

    end if;
  end Prune;

  ---------------------------------------------------------------------------

  procedure Clear_Leaf_Nodes is
  begin
    clear_list := (others => False);
    --  Remove all leaf nodes by recursively pruning subtrees
    for node in 0 .. 255 loop
      Prune (node);
    end loop;
    --  Next, re-initialize our list of free table entries
    next_free := table_last + 1;  --  !! Assumes a full table !!
    for node in reverse Shrink.first_entry .. table_last loop
      if clear_list (node) then
        next_free := next_free - 1;
        free_list (next_free) := node;
      end if;
    end loop;
  end Clear_Leaf_Nodes;

  ---------------------------------------------------------------------------

  procedure Table_Add (prefix_initially : Natural; suffix : Natural) is
    free_node : Natural;
    prefix : Natural := prefix_initially;
  begin
    if next_free <= table_last then
      free_node := free_list (next_free);
      next_free := next_free + 1;
      table (free_node) :=
        (child   => Shrink.unused,
         sibling => Shrink.unused,
         suffix  => suffix);
      if table (prefix).child = Shrink.unused then
        table (prefix).child := free_node;
      else
        prefix := table (prefix).child;
        while table (prefix).sibling /= Shrink.unused loop
          prefix := table (prefix).sibling;
        end loop;
        table (prefix).sibling := free_node;
      end if;
    end if;
  end Table_Add;

  ---------------------------------------------------------------------------
  --  Search for a Prefix:Suffix pair in our Symbol table. If found, return
  --  the index value where found.  If not found, return False and set
  --  found_at to UNUSED.
  ---------------------------------------------------------------------------
  procedure Table_Lookup
    (target_prefix : in     Integer;
     target_suffix : in     Natural;
     found_at      :    out Integer;
     found         :    out Boolean)
  is
    idx : Integer := table (target_prefix).child;
  begin
    --  Lookup an entry in the table. If found, return True and set
    --  parameter found_at with the index of the entry at which the match
    --  was found. If not found, return False and plug `unused` into found_at.
    --
    if idx = Shrink.unused then
      --  No child.
      found_at := Shrink.unused;
      found := False;
    else
      --  Check the children.
      loop
        if table (idx).suffix = target_suffix then
          found_at := idx;
          found := True;
          return;
        else
          idx := table (idx).sibling;
          if idx = Shrink.unused then
            found_at := Shrink.unused;
            found := False;
            return;
          end if;
        end if;
      end loop;
    end if;
  end Table_Lookup;

  ---------------------------------------------------------------------------
  --  The actual Crunching algorithm
  ---------------------------------------------------------------------------

  last_code : Integer := 0;
  is_first_atom : Boolean := True;
  current_max_code : Natural;  --  Largest code that can be written in code_size bits

  procedure Shrink_Atom (suffix : Integer) is
    where_found  : Integer;
    is_lookup_ok : Boolean;
  begin
    if is_first_atom then
      --  If just getting started ...
      bit_buffer := 0;
      valid_bits := 0;
      code_size  := Shrink.minimum_code_bit_size;
      current_max_code := 2 ** code_size - 1;
      last_code  := suffix;    --    get first character from input,
      is_first_atom := False;  --    and reset the first char flag.
    elsif suffix = Shrink.unused then
      --  Nothing to crunch... must be EOF on input
      Put_Code (last_code);         --  Write last prefix code
      Flush_Bit_Buffer;
      Flush_Output;
    elsif Is_Table_Full then
      Put_Code (last_code);
      --  NB: PKZip's LZW variant does not necessarily clear the table
      --  when it is full. Hence the need for the special code below.
      Put_Code (Shrink.special_code);
      Put_Code (Shrink.code_for_clearing_table);
      Clear_Leaf_Nodes;
      Table_Add (last_code, suffix);
      last_code := suffix;
    else
      Table_Lookup (last_code, suffix, where_found, is_lookup_ok);

      if is_lookup_ok then
        --  If last_code:suffix pair is found in the code table, then ...
        --  ... set last_code to the entry where the pair is located
        last_code := where_found;
        --  No code is emitted!
      else
        --  Not in table
        Put_Code (last_code);           --  Write current last_code code
        Table_Add (last_code, suffix);  --  Attempt to add to code table
        last_code := suffix;            --  Reset last_code code for new char

        if code_size < Shrink.maximum_code_bit_size
          and then (not Is_Table_Full)
          and then free_list (next_free) > current_max_code
        then
          --  Time to increase the code size and change the maximum code
          Put_Code (Shrink.special_code);
          Put_Code (Shrink.code_for_increasing_code_size);
          code_size := code_size + 1;
          current_max_code := 2 **  code_size - 1;
        end if;

      end if;

    end if;
  end Shrink_Atom;

  feedback_milestone,
  bytes_in : Zip_Streams.ZS_Size_Type := 0;   --  Count of input file bytes processed

  procedure Process_Input (source : Byte_Buffer) is
    pct_done : Natural;
    user_aborting : Boolean;
    last_processed : Integer := source'First - 1;
    use Zip_Streams;
  begin
    if source'Length < 1 then
      Shrink_Atom (Shrink.unused);
    else

      for i in source'Range loop
        bytes_in := bytes_in + 1;

        if feedback /= null then
          if bytes_in = 1 then
            feedback (0, False, user_aborting);
          end if;

          if feedback_milestone > 0 and then --  Bugfix GdM 23-Dec-2002
             ((bytes_in - 1) mod feedback_milestone = 0
              or bytes_in = ZS_Size_Type (input_size))
          then
            if input_size_known then
              pct_done := Integer ((100.0 * Float (bytes_in)) / Float (input_size));
              feedback (pct_done, False, user_aborting);
            else
              feedback (0, False, user_aborting);
            end if;
            if user_aborting then
              raise User_abort;
            end if;
          end if;

        end if;

        Shrink_Atom (Integer (source (i)));

        last_processed := i;
        if input_size_known and then bytes_in >= ZS_Size_Type (input_size) then
          --  The job is done, even though there are more in the buffer
          IO_buffers.InputEoF := True;
          exit;
        end if;
      end loop;

      Zip.CRC_Crypto.Update (CRC, source (source'First .. last_processed));
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
  Initialize_Data_Structures;
  output_size := 0;
  --
  begin
    Read_Block (IO_buffers, input);
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
    compression_ok := bytes_in > 0;

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
