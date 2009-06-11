with Ada.Text_IO;                       use Ada.Text_IO;
with UnZip.Streams;                     use UnZip.Streams;
with Zip;

procedure Test_UnZ_Streams is
  f: Zipped_File_Type;
  s: Stream_Access;
  c: Character;
  a: constant String:= "tuttifru.zip"; -- Archive created by test_uza.cmd
  n: constant String:= "$12Defl5.tmp"; -- Compressed file in that archive
begin
  Open( File           => f,
        Archive_Name   => a,
        Name           => n,
        Password       => "tralala",
        Case_Sensitive => False
  );
  s:= Stream(f);
  while not End_of_file(f) loop -- We just output the contents of file
    Character'Read(s,c);        -- named in 'n' to standard output
    Put(c);
  end loop;
  Close(f);
exception
  when Zip.Zip_file_open_error =>
    Put( "Can't open archive [" & a & ']' );
  when Zip.File_name_not_found =>
    Put( "Cannot find [" & n & "] in archive [" & a & ']' );
  when UnZip.Wrong_password      =>
    Put( "Password doesn't fit!" );
end Test_UnZ_Streams;
