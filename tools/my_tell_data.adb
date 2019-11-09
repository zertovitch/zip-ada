------------------------------------------------------------------------------
--  File:            Myteldat.adb or My_tell_data.adb
--  Description:     Part of the UnZipAda demo
------------------------------------------------------------------------------

with UnZip, My_dots, Summary;

with Ada.Text_IO;                       use Ada.Text_IO;

with Interfaces;                        use Interfaces;

procedure My_tell_data
             (file_name          : String;
              compressed_bytes   : UnZip.File_size_type;
              uncompressed_bytes : UnZip.File_size_type;
              method             : UnZip.PKZip_method) is

  package MIO is new Modular_IO (UnZip.File_size_type);

  function CutName (n : String; l : Natural) return String is
    dots : constant String := "...";
  begin
    if n'Length > l then
      return dots & n (n'Last - (l - 1) + dots'Length .. n'Last);
    else
      return n;
    end if;
  end CutName;

begin
  New_Line;
  if Summary.total_entries = 0 then
    Put_Line (" Name                      Method    Compressed size      Uncompressed size");
    Put_Line (" ------------------------- --------- ---------------      -----------------");
  end if;
  Put (' ');
  My_dots.done_dots := 0;
  declare
    maxlen : constant := 24;
    cut : constant String := CutName (file_name, maxlen);
  begin
    Put (cut);
    for l in cut'Length .. maxlen loop
      Put (' ');
    end loop;
  end;
  Put (' ' & Summary.Nice_image (method));
  MIO.Put (compressed_bytes, 10);
  if uncompressed_bytes = 0 then
    Put (" :         ");
  else
    Put (" :");
    MIO.Put (
      UnZip.File_size_type (
        (100.0 * Long_Float (compressed_bytes)) / Long_Float (uncompressed_bytes)
      ), 4);
    Put ("% of ");
  end if;
  MIO.Put (uncompressed_bytes, 10);
  Put (' ');
  --  We summarize here the length of processed files
  Summary.total_uncompressed :=
    Summary.total_uncompressed + uncompressed_bytes;
  Summary.total_compressed :=
    Summary.total_compressed   + compressed_bytes;
  Summary.total_entries := Summary.total_entries + 1;
  --  Per-method statistics:
  Summary.files_per_method (method) := Summary.files_per_method (method) + 1;
  Summary.uncompressed_per_method (method) := Summary.uncompressed_per_method (method) + uncompressed_bytes;
  Summary.compressed_per_method (method) := Summary.compressed_per_method (method) + compressed_bytes;
end My_tell_data;
