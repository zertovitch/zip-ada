--  Get the time needed to create entries, including check for duplicates.
--  Compression time is not in the focus here.

with Zip, Ada.Calendar, Ada.Text_IO;
with Zip.Compress;
with Zip.Create;                        use Zip.Create;

with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;

procedure Test_Zip_Create_Info_Timing is
  t1, t2 : Ada.Calendar.Time;
  use Ada.Calendar;

  procedure Create_with_many (n : Positive; duplicates : Zip.Duplicate_name_policy) is
    stream  : aliased Zip_File_Stream;
    archive : Zip_Create_Info;

    procedure Add_one_entry (file_name : String) is
    begin
      Zip.Create.Add_String (
        Info              => archive,
        Contents          => "ABCD",
        Name_in_archive   => file_name
      );
    end Add_one_entry;

    function Leading_zeros (i, zeros : Integer) return String is
      pad : constant Integer := 10 ** zeros;
      str : String (1 .. zeros + 2);
    begin
      Put (str, i + pad);
      return str (3 .. str'Last);
    end Leading_zeros;

  begin
    Create_Archive (
      archive,
      stream'Unchecked_Access,
      "test_create.zip",
      Zip.Compress.Store,
      --  NB: for creation, the value of parameter "duplicates" may impact performance.
      duplicates
    );
    for i in 1 .. n loop
      Add_one_entry (
        "Entry #" & Leading_zeros (i, 5) & ".txt"
      );
    end loop;
    Finish (archive);
  end Create_with_many;
begin
  for d in Zip.Duplicate_name_policy loop
    Ada.Text_IO.Put_Line (
      "Creating Zip archive with many entries; check for duplicate names = " &
      Zip.Duplicate_name_policy'Image (d)
    );
    t1 := Ada.Calendar.Clock;
    Create_with_many (2 ** 16 - 1, d);
    t2 := Ada.Calendar.Clock;
    Ada.Text_IO.Put_Line (
      "Time elapsed for creating Zip file:" &
      Duration'Image (t2 - t1) &
      " seconds"
    );
  end loop;
end Test_Zip_Create_Info_Timing;
