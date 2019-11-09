with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with UnZip.Streams;                     use UnZip.Streams;
with Zip;

procedure Test_UnZ_Streams is

  --  Test archive extraction as an *input* stream.

  procedure Test_Input_Stream is
    f : Zipped_File_Type;
    s : UnZip.Streams.Stream_Access;
    c : Character;
    a : constant String := "tuttifru.zip";  --  Archive created by test_uza.cmd
    n : constant String := "$12Defl5.tmp";  --  Compressed file in that archive
  begin
    Open (File           => f,
          Archive_Name   => a,
          Name           => n,
          Password       => "tralala",
          Case_sensitive => False
    );
    s := Stream (f);
    while not End_Of_File (f) loop  --  We just output the contents of file
      Character'Read (s, c);        --  named in 'n' to standard output
      Put (c);                      --  character by character
    end loop;
    Close (f);
    --
  exception
    when Zip.Archive_open_error =>
      Put_Line ("Can't open archive [" & a & ']');
    when Zip.Entry_name_not_found =>
      Put_Line ("Cannot find [" & n & "] in archive [" & a & ']');
    when UnZip.Wrong_password      =>
      Put_Line ("Password doesn't fit!");
  end Test_Input_Stream;

  --  Test archive extraction as an *output* stream.

  procedure Test_Output_Stream (suffix : String; trash_dir : Boolean) is
    o : Ada.Streams.Stream_IO.File_Type;
    z : Zip.Zip_info;
    a : constant String := "detailed_results.zip";               --  Created by Demo_csv_into_zip
    n : constant String := "flood/oveRSEas/auSTRalasia_fd.csv";  --  we check case-unsensitiveness
  begin
    Create (o, Out_File, "demo_data_" & suffix & ".csv");
    Zip.Load (
      info           => z,
      from           => a
    );
    Extract (
      Destination      => Stream (o).all,
      Archive_Info     => z,
      Entry_Name       => n,
      Ignore_Directory => trash_dir
    );
    Close (o);
  exception
    when Zip.Archive_open_error =>
      Put_Line ("Can't open archive [" & a & ']');
    when Zip.Entry_name_not_found =>
      Put_Line ("Cannot find [" & n & "] in archive [" & a & ']');
  end Test_Output_Stream;

begin
  Test_Input_Stream;
  Test_Output_Stream ("with_dir", False);
  Test_Output_Stream ("without_dir", True);
end Test_UnZ_Streams;
