with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with UnZip.Streams;                     use UnZip.Streams;
with Zip;

procedure Test_UnZ_Streams is

  procedure Test_Input_Stream is
    f: Zipped_File_Type;
    s: UnZip.Streams.Stream_Access;
    c: Character;
    a: constant String:= "tuttifru.zip"; -- Archive created by test_uza.cmd
    n: constant String:= "$12Defl5.tmp"; -- Compressed file in that archive
  begin
    Open( File           => f,
          Archive_Name   => a,
          Name           => n,
          Password       => "tralala",
          Case_sensitive => False
    );
    s:= Stream(f);
    while not End_Of_File(f) loop -- We just output the contents of file
      Character'Read(s,c);        -- named in 'n' to standard output
      Put(c);
    end loop;
    Close(f);
    --
  exception
    when Zip.Zip_file_open_Error =>
      Put( "Can't open archive [" & a & ']' );
    when Zip.File_name_not_found =>
      Put( "Cannot find [" & n & "] in archive [" & a & ']' );
    when UnZip.Wrong_password      =>
      Put( "Password doesn't fit!" );
  end Test_Input_Stream;

  procedure Test_Output_Stream is
    o: Ada.Streams.Stream_IO.File_Type;
    z: Zip.Zip_info;
    a: constant String:= "detailed_results.zip"; -- Created by Demo_csv_into_zip
    n: constant String:= "flood/overseas/australasia_fd.csv";
  begin
    Create(o, Out_File, "demo_data.csv");
    Zip.Load(
      info           => z,
      from           => a
    );
    Extract(
      Destination  => Stream(o).all,
      Archive_Info => z,
      Name         => n
    );
    Close(o);
  exception
    when Zip.Zip_file_open_Error =>
      Put( "Can't open archive [" & a & ']' );
    when Zip.File_name_not_found =>
      Put( "Cannot find [" & n & "] in archive [" & a & ']' );
  end Test_Output_Stream;

begin
  Test_Output_Stream;
  Test_Input_Stream;
end Test_UnZ_Streams;
