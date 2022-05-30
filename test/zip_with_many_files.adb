------------------------------------------------------------------------------
--  File:            Zip_with_many_files.adb
--  Description:     Demo/test:
--                     - stuff a large number of files into a .zip file
--                     - test the (2 ** 16 - 2) limit of the Zip_32 archive
--                         format and the automatic promotion to Zip_64.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip.Create;                        use Zip.Create;

with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Zip_with_many_files is

  procedure Create_with_many (n : Positive) is
    stream  : aliased Zip_File_Stream;
    archive : Zip_Create_Info;

    procedure Add_one_entry (file_name : String; rep : Natural) is
    begin
      Zip.Create.Add_String (
        Info              => archive,
        Contents          => "..." & rep * ("Hello! My name is: """ & file_name & '"' & ASCII.LF),
        Name_in_archive   => file_name,
        Creation_time     => use_clock
      );
    end Add_one_entry;

    function Leading_zeros (i, zeros : Integer) return String is
      pad : constant Integer := 10 ** zeros;
      str : String (1 .. zeros + 2);
    begin
      Put (str, i + pad);
      return str (3 .. str'Last);
    end Leading_zeros;

    n_img : constant String := Integer'Image (n);

    procedure Create_with_Trace (file_name : String) is
    begin
      Ada.Text_IO.Put_Line (file_name);
      Create_Archive
       (archive,
        stream'Unchecked_Access,
        file_name);
    end Create_with_Trace;

  begin
    Create_with_Trace ("many_" & n_img (n_img'First + 1 .. n_img'Last) & ".zip");
    for i in 1 .. n loop
      Add_one_entry (
        "Entry #" & Leading_zeros (i, 5) & ".txt",
        Integer'Max (0, i / 100 - 10)
      );
    end loop;
    Finish (archive);
  end Create_with_many;

begin
  Create_with_many (2 ** 12);
  Create_with_many (2 ** 13);
  Create_with_many (2 ** 14);
  Create_with_many (2 ** 15);
  Create_with_many (2 ** 16 - 2);
  Create_with_many (2 ** 16 - 1);  --  Should promote archive to Zip_64 mode.
end Zip_with_many_files;
