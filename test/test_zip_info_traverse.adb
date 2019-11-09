--  Check duplicate-tolerant traversal

with Zip, Ada.Command_Line, Ada.Text_IO;

procedure Test_Zip_Info_Traverse is
  use Zip, Ada.Command_Line, Ada.Text_IO;
  zi : Zip_info;
  procedure Action (entry_name : String) is
  begin
    Put_Line (entry_name);
  end Action;
  procedure Listing is new Traverse (Action);
begin
  Load (zi, Argument (1), duplicate_names => admit_duplicates);
  Listing (zi);
end Test_Zip_Info_Traverse;
