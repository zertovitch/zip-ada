------------------------------------------------------------------------------
--  File:            Zip_with_many_files.adb
--  Description:     Demo/test:
--                     - stuff a large number of files into a .zip file
--                     - test 65535 limit of "ZIP32" archive format
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip.Create;                        use Zip.Create;

with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

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

  begin
    Create_Archive (
      archive,
      stream'Unchecked_Access,
      n_img (n_img'First + 1 .. n_img'Last) & ".zip"
    );
    for i in 1 .. n loop
      Add_one_entry (
        "Entry #" & Leading_zeros (i, 5) & ".txt",
        Integer'Max (0, i / 100 - 10)  --  Obtain a certain number of incompressible entries.
      );
    end loop;
    Finish (archive);
  end Create_with_many;
begin
  Create_with_many (2 ** 12);
  Create_with_many (2 ** 13);
  Create_with_many (2 ** 14);
  Create_with_many (2 ** 15);
  Create_with_many (2 ** 16 - 1);
  Create_with_many (2 ** 16);  --  Should raise Zip_Capacity_Exceeded in Zip_32 mode.
end Zip_with_many_files;
