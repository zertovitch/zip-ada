with Ada.Containers.Vectors,
     Ada.Command_Line,
     Ada.Directories,
     Ada.Numerics.Generic_Elementary_Functions,
     Ada.Sequential_IO,
     Ada.Text_IO;

procedure Entropy_Segmentation is

  type Index is new Integer;
  subtype Positive_Index is Index range 1 .. Index'Last;
  package Index_Vectors is new Ada.Containers.Vectors (Positive, Positive_Index);

  type Byte is mod 256;
  package Byte_Buffers is new Ada.Containers.Vectors (Positive_Index, Byte);

  buffer : Byte_Buffers.Vector;

  package BIO is new Ada.Sequential_IO (Byte);

  f_in, f_seg : BIO.File_Type;
  f_csv : Ada.Text_IO.File_Type;

  procedure Segment (seg : out Index_Vectors.Vector) is
    type Real is digits 15;
    package REF is new Ada.Numerics.Generic_Elementary_Functions (Real);
    use REF;
    discrepancy_threshold : constant := 2.0;      --  Discrepancy detection threshold.
    index_threshold       : constant := 200_000;  --  Do segmentation only above a certain distance.
    window_size           : constant := 50_000;   --  Sliding window size.
    reporting_period : constant := 1000;
    inv_window_size : constant Real := 1.0 / Real (window_size);
    len : constant Index := Index (buffer.Length);
    seg_point : Index;
    freq : array (Byte) of Natural := (others => 0);
    elem : array (Byte) of Real := (others => 0.0);
    function Entropy_Function (p : Real) return Real is (-p * Log (p));
    function Prob (b : Byte) return Real is (Real (freq (b)) * inv_window_size);
    entropy : Real := 0.0;
    entropy_mark : Real;
    index_mark : Index := 1;
    p : Real;
    bt : Byte;
  begin
    seg.Clear;
    for i in 1 .. len loop
      --  Fill the sliding window.
      bt := buffer (i);
      freq (bt) := freq (bt) + 1;
      if i = window_size then
        --  Compute initial entropy value.
        for b in Byte loop
          p := Prob (b);
          if p > 0.0 then
            elem (b) := Entropy_Function (p);
            entropy := entropy + elem (b);
          end if;
        end loop;
        entropy_mark := entropy;
      elsif i > window_size then
        --  Adjust entropy for new value coming in.
        entropy := entropy - elem (bt);
        p := Prob (bt);
        --  Note: count (bt) is positive, px too.
        elem (bt) := Entropy_Function (p);
        entropy := entropy + elem (bt);
        --  Adjust entropy for old value disappearing.
        bt := buffer (i - window_size);
        entropy := entropy - elem (bt);
        freq (bt) := freq (bt) - 1;
        p := Prob (bt);
        if p > 0.0 then
          elem (bt) := Entropy_Function (p);
          entropy := entropy + elem (bt);
        else
          elem (bt) := 0.0;
        end if;
        if abs (entropy - entropy_mark) > discrepancy_threshold then
          seg_point := i - window_size;
          if seg_point - index_mark > index_threshold then
            seg.Append (seg_point);
            index_mark := seg_point;
            entropy_mark := entropy;
          end if;
        end if;
        if i mod reporting_period = len mod reporting_period then
          Ada.Text_IO.Put_Line (f_csv, entropy'Image);
        end if;
      end if;
    end loop;
    seg.Append (len);
  end Segment;

  use Ada.Command_Line, Ada.Directories, Ada.Text_IO, BIO;

  function Read_Byte return Byte is
    b : Byte;
  begin
    Read (f_in, b);
    return b;
  end Read_Byte;

  function More_Bytes return Boolean is
  begin
    return not End_Of_File (f_in);
  end More_Bytes;

  seg : Index_Vectors.Vector;
  index_0 : Index := 1;

begin
  Put_Line ("Entropy_Segmentation");
  New_Line;
  Put_Line ("entropy_segmentation.adb [input file]");
  New_Line;
  Put_Line ("Outputs:");
  Put_Line ("          *.csv: entropy values");

  if Argument_Count < 1 then
    Open (f_in, In_File, "zip_lib/bzip2-encoding.adb");
  else
    Open (f_in, In_File, Argument (1));
  end if;

  while More_Bytes loop
    buffer.Append (Read_Byte);
  end loop;

  Create (f_csv, Out_File, "dump_" & Simple_Name (Name (f_in)) & ".csv");
  Put_Line (f_csv, "Entropy");

  Segment (seg);

  if Integer (seg.Length) > 1 then
    --  Slice the input file:
    for i in 1 .. Integer (seg.Length) loop
      declare
        imi : String := i'Image;
      begin
        imi (imi'First) := '_';
        Create (f_seg, Out_File, "seg" & imi & '_' & Simple_Name (Name (f_in)) & ".seg");
      end;
      for x in index_0 .. seg (i) loop
        Write (f_seg, buffer (x));
      end loop;
      Close (f_seg);
      index_0 := seg (i) + 1;
    end loop;

    --  Some info:
    for s of seg loop
      Put_Line
        ("Segment limit at" & s'Image);
    end loop;
    Put ("Non-trivial segmentation - Press return");
    Skip_Line;
  end if;

  Close (f_in);
  Close (f_csv);
end Entropy_Segmentation;
