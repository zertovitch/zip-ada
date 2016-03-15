------------------------------------------------------------------------------
--  File:            Zip_with_many_files.adb
--  Description:     Demo/test: stuff a large number of files into a .zip file
--  Date/version:    25-Feb-2010
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip_Streams;                       use Zip_Streams;
with Zip.Create;                        use Zip.Create;

procedure Zip_with_many_files is
  stream  : aliased File_Zipstream;
  archive : Zip_Create_info;

  procedure Add_one_entry(to_file: String) is
  begin
    Zip.Create.Add_String(
      Info              => archive,
      Contents          => "Hello! My name is: """ & to_file & '"',
      Name_in_archive   => to_file
    );
  end Add_one_entry;

  some_date : constant Time:= Calendar.Time_Of(2016, 3, 14);

begin
  Create(archive, stream'Unchecked_Access, "Large.zip", some_date);
  for i in 1..65_535 loop
    Add_one_entry( "Entry #" & Integer'Image(i) & ".txt");
  end loop;
  Finish(archive);
end Zip_with_many_files;
