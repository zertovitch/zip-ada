with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Unchecked_Deallocation;

with Suffix_Arrays;

package body BWT is

  --  "Dumb" encoder corresponding to the academic representation
  --  of the algorithm, with a n*n matrix which is sorted
  --  row-wise.
  --
  procedure Encode_Dumb (message : in out String; index : out Positive) is
    subtype Msg_Range is Integer range message'Range;
    subtype Message_Clone is String (Msg_Range);

    --  The table will contain all rotations of the message.
    --  If the length is n, the table will have the size n^2.
    --  This "visual" version of the algorithm is a massive
    --  waste of space (and time too)...

    type Table is array (Msg_Range) of Message_Clone;

    --  Access type needed only because of Ada systems with
    --  tiny stack sizes or complicated stack options.

    type p_Table is access Table;
    procedure Dispose is new Ada.Unchecked_Deallocation (Table, p_Table);

    procedure String_Sort is new Ada.Containers.Generic_Constrained_Array_Sort
      (Index_Type   => Msg_Range,
       Element_Type => Message_Clone,
       Array_Type   => Table);

    m : p_Table := new Table;
    found : Boolean := False;
    new_message : Message_Clone;
  begin
    --  Fill table m with rotated copies of message.
    for i in Msg_Range loop
      for j in Msg_Range loop
        m (i)(j) := message (Msg_Range'First + (j - Msg_Range'First + i - Msg_Range'First) mod message'Length);
      end loop;
    end loop;
    String_Sort (m.all);
    for i in Msg_Range loop
      --  Copy last column into transformed message:
      new_message (i) := m (i)(Msg_Range'Last);
      if not found and then m (i) = message then
        --  Found the row index of the original message.
        found := True;
        index := i;
      end if;
    end loop;
    Dispose (m);
    message := new_message;
  end Encode_Dumb;

  --  "Smart" encoder: the rotated strings are not stored.
  --  We only set up an array of offsets.
  --
  procedure Encode_Smart (message : in out String; index : out Positive) is
    length : constant Natural := message'Length;

    subtype Offset_Range is Integer range 0 .. length - 1;
    type Offset_Table is array (Offset_Range) of Offset_Range;

    --  Compare the message, rotated with two (possibly different) offsets.
    function Lexicographically_Smaller (left, right : Offset_Range) return Boolean is
      l, r : Character;
    begin
      for i in Offset_Range loop
        l := message (message'First + (i - left)  mod length);
        r := message (message'First + (i - right) mod length);
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

    offset : Offset_Table;
    new_message : String (message'Range);
  begin
    --  At the beginning, row i (0-based) of the matrix represents
    --  a rotation of offset i of the original message (row 0 has a
    --  0 offset, row 1 rotates the message by 1 character, etc.):
    --
    for i in Offset_Range loop
      offset (i) := i;
    end loop;
    Offset_Sort (offset);
    for i in Offset_Range loop
      --  Copy last column into transformed message:
      new_message (message'First + i) :=
        message (message'First + (length - 1 - offset (i)) mod length);
      if offset (i) = 0 then
        --  Found the row index of the original message.
        index := 1 + i;
      end if;
    end loop;
    message := new_message;
  end Encode_Smart;

  procedure Encode_Smarter (message : in out String; index : out Positive) is
    type Byte is mod 2 ** 8;
    type Byte_Array is array (Integer range <>) of Byte;

    package Byte_SA is new
      Suffix_Arrays
        (Symbol => Byte, Count_Type => Integer, Symbol_Array => Byte_Array);

    --  We duplicate the message in order to simulate its periodicity.
    es : Byte_Array (1 .. 2 * message'Length);
    sa : Byte_SA.Sa_Type (es'Range);

    j, i_first_copy : Integer;

    new_message : String (message'Range);

  begin
    j := 0;
    for i in message'Range loop
      j := j + 1;
      es (j) := Character'Pos (message (i));
      --  Duplicate / simulate wrap-around, periodic message for BWT:
      es (message'Length + j) := es (j);
    end loop;
    --
    Byte_SA.Build_Sa (es, sa);
    --
    i_first_copy := 0;
    for i in sa'Range loop
      j := Byte_SA.Suffix (sa, i);
      if j <= message'Length then
        --  Consider only suffixes starting on the
        --  first copy of the message.
        i_first_copy := i_first_copy + 1;
        --  Now, we jump on the last column of the BWT matrix, from the first column.
        if j = 1 then
          --  The column index points to the first element;
          --  The suffix truncated to message'Length is the whole, original message.
          j := message'Length;
          index := i_first_copy;  --  Remember the row index.
        else
          j := j - 1;
        end if;
        --  BWT-transformed message.
        new_message (i_first_copy) := message (j);
      end if;
    end loop;
    message := new_message;
  end Encode_Smarter;

  procedure Encode (message : in out String; index : out Positive; method : Encoding_Method) is
  begin
    if message'Length = 0 then
      index := 1;
      return;
    else
      case method is
        when matrix_sorting => Encode_Dumb (message, index);
        when index_sorting  => Encode_Smart (message, index);
        when suffix_array   => Encode_Smarter (message, index);
      end case;
    end if;
  end Encode;

  --  Very dumb, but illustrative, decoder.
  --
  procedure Decode (message : in out String; index : in Positive) is
    subtype Msg_Range is Integer range message'Range;
    subtype Message_Clone is String (Msg_Range);

    type Table is array (Msg_Range) of Message_Clone;

    --  Access type needed only because of Ada systems with
    --  tiny stack sizes or complicated stack options.
    type p_Table is access Table;
    procedure Dispose is new Ada.Unchecked_Deallocation (Table, p_Table);

    procedure Sort is
      new Ada.Containers.Generic_Constrained_Array_Sort
        (Index_Type   => Msg_Range,
         Element_Type => Message_Clone,
         Array_Type   => Table);

    m : p_Table := new Table'(others => (others => ' '));

  begin

    if message'Length = 0 then
      return;
    end if;

    Shift_Insert_Sort :
    for iter in Msg_Range loop
      --  Shift columns right
      for i in Msg_Range loop
        for j in reverse Msg_Range'First + 1 .. Msg_Range'Last loop
          m (i)(j) := m (i)(j - 1);
        end loop;
      end loop;
      --  Insert transformed string t as first column (again and again).
      --
      --  The miracle: after iteration #1, t(i) is the correct predecessor
      --  of the character on sorted partial row i (1 character).
      --  This gives the full list of pairs.
      --
      --  After 2nd sorting (end of iteration #2), t(i) is also the correct
      --  predecessor each sorted pair.
      --  We have then the list of all triplets. And so on.
      --
      for i in Msg_Range loop
        m (i)(1) := message (i);
      end loop;
      Sort (m.all);
    end loop Shift_Insert_Sort;
    --  After iteration n we have a sorted list of all rotated
    --  versions of the original string. The table is identical
    --  to the table after encoding.
    --  The original string is at row 'index'.
    message := m (index);
    Dispose (m);
  end Decode;

end BWT;
