--  This test builds with ObjectAda and GNAT in -gnat83 mode, so
--  perhaps with all Ada (83, 95, 2005, ...) compilers!...

with Length_limited_Huffman_code_lengths;

with Text_IO; use Text_IO;

procedure Test_LLHC is
  package IIO is new Integer_IO(Integer); use IIO;
  subtype Alphabet is Character range 'a'..'k';
  type Alpha_Array is array(Alphabet) of Natural;
  freq, len: Alpha_Array;
begin
  freq:= (10, 30, 12, 5, 17, 20, 17, 0, 20, 0, 15);
  for m in 4..5 loop
    declare
      procedure LLHCL is new
        Length_limited_Huffman_code_lengths(Alphabet, Natural, Alpha_Array, Alpha_Array, m);
    begin
      LLHCL(freq, len);
    end;
    Put("Maximum Huffman code length (constraint):");
    Put(m, 3);
    Put_Line(" bits");
    Put_Line("------------------------------------");
    for c in len'Range loop
      Put("    " & c & " freq =");
      Put(freq(c), 3);
      Put(" length =");
      Put(len(c), 3);
      New_Line;
    end loop;
  end loop;
end Test_LLHC;
