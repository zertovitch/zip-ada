with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Unchecked_Deallocation;

package body BWT is

  procedure Encode (message : in out String; index : out Positive) is
    subtype Rng is Integer range message'Range;
    subtype Message_Clone is String (Rng);
    type Table is array (Rng) of Message_Clone;
    --  Access type needed only because of Ada systems with
    --  tiny stack sizes or complicated stack options.
    type p_Table is access Table;
    procedure Dispose is new Ada.Unchecked_Deallocation (Table, p_Table);
    --
    procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort (
      Index_Type   => Rng,
      Element_Type => Message_Clone,
      Array_Type   => Table);
    m : p_Table := new Table;
    found : Boolean := False;
    new_message : Message_Clone;
  begin
    --  Fill table m with rotated copies of message.
    for i in Rng loop
      for j in Rng loop
        m (i)(j) := message (Rng'First + (j - Rng'First + i - Rng'First) mod message'Length);
      end loop;
    end loop;
    Sort (m.all);
    --  Copy last column and find index of original message.
    for i in Rng loop
      new_message (i) := m (i)(Rng'Last);
      if not found and then m (i) = message then
        found := True;
        index := i;  --  Found row with the message without rotation.
      end if;
    end loop;
    Dispose (m);
    message := new_message;
  end Encode;

  procedure Decode (message : in out String; index : in Positive) is
    subtype Rng is Integer range message'Range;
    subtype Message_Clone is String (Rng);
    type Table is array (Rng) of Message_Clone;
    --  Access type needed only because of Ada systems with
    --  tiny stack sizes or complicated stack options.
    type p_Table is access Table;
    procedure Dispose is new Ada.Unchecked_Deallocation (Table, p_Table);
    --
    procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort (
      Index_Type   => Rng,
      Element_Type => Message_Clone,
      Array_Type   => Table);
    m : p_Table := new Table'(others => (others => ' '));
  begin
    Shift_Insert_Sort :
    for iter in Rng loop
      --  Shift columns right
      for i in Rng loop
        for j in reverse Rng'First + 1 .. Rng'Last loop
          m (i)(j) := m (i)(j - 1);
        end loop;
      end loop;
      --  Insert transformed string t as first column (again and again).
      --
      --  The miracle: after iteration #1, t(i) is the correct predecessor
      --  of the character on sorted row i. This gives the full list of pairs.
      --  After 2nd sorting (end of iteration #2), t(i) is also the correct
      --  predecessor each sorted pair.
      --  We have then the list of all triplets. And so on.
      --
      for i in Rng loop
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
