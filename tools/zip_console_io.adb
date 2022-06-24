--  Console I/O for ZipAda, UnZipAda and ReZip tools
--  It's not nice code (global variables), so please don't use it elsewhere.

with Ada.Text_IO;

package body Zip_Console_IO is

  package body Summary is

    procedure Reset is
    begin
      total_uncompressed      := 0;
      total_compressed        := 0;
      total_entries           := 0;
      files_per_method        := (others => 0);
      uncompressed_per_method := (others => 0);
      compressed_per_method   := (others => 0);
    end Reset;

    function Nice_image (format : Zip.PKZip_method) return String is
      img_stuffed : String (1 .. Zip.PKZip_method'Width) := (others => ' ');
      img : constant String := Zip.Image (format);
    begin
      img_stuffed (1 .. img'Length) := img;
      return img_stuffed;
    end Nice_image;

  end Summary;

  dots : constant := 8;
  done_dots : Natural := 0;

  procedure My_feedback
   (percents_done :  in Natural;
    entry_skipped :  in Boolean;
    user_abort    : out Boolean)
  is
    new_done_dots : constant Natural := (dots * percents_done) / 100;
    use Ada.Text_IO;
  begin
    if entry_skipped then
      Put ("-skipped-");
    else
      for i in done_dots + 1 .. new_done_dots loop
        if i = 1 then
          Put ('[');
        end if;
        Put ('.');
        if i = dots then
          Put (']');
        end if;
      end loop;
      done_dots := new_done_dots;
    end if;
    user_abort := False; -- pointless in this command-line version (Ctrl-C is ok)
  end My_feedback;

  procedure My_tell_data
   (file_name          : String;
    compressed_bytes   : Zip.Zip_64_Data_Size_Type;
    uncompressed_bytes : Zip.Zip_64_Data_Size_Type;
    method             : Zip.PKZip_method)
  is
    use Ada.Text_IO;

    package MIO is new Modular_IO (Zip.Zip_64_Data_Size_Type);

    function Cut_name (n : String; l : Natural) return String is
      three_dots : constant String := "...";
    begin
      if n'Length > l then
        return three_dots & n (n'Last - (l - 1) + three_dots'Length .. n'Last);
      else
        return n;
      end if;
    end Cut_name;

    use type Zip.Zip_64_Data_Size_Type;

  begin
    New_Line;
    if Summary.total_entries = 0 then
      Put_Line (" Name                      Method    Compressed size      Uncompressed size");
      Put_Line (" ------------------------- --------- ---------------      -----------------");
    end if;
    Put (' ');
    done_dots := 0;
    declare
      maxlen : constant := 24;
      cut : constant String := Cut_name (file_name, maxlen);
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
        Zip.Zip_64_Data_Size_Type (
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

  procedure My_resolve_conflict
   (file_name       :  in String;
    name_encoding   :  in Zip.Zip_name_encoding;
    action          : out UnZip.Name_conflict_intervention;
    new_name        : out String;
    new_name_length : out Natural)
  is
    pragma Unreferenced (name_encoding);
    c : Character;
    use Ada.Text_IO, UnZip;
  begin
    loop
      New_Line;
      Put_Line ("File " & file_name & " already exists.");
      Put (" Overwrite ?  (y)es / (n)o / (A)ll / (N)one / (r)ename / (q)uit ");
      Get_Immediate (c);
      Put_Line ("-> " & c);
      exit when c = 'y' or c = 'n' or c = 'A' or c = 'N' or c = 'r' or c = 'q';
    end loop;
    case c is
      when 'y'       => action := yes;
      when 'n'       => action := no;
      when 'A'       => action := yes_to_all;
      when 'N'       => action := none;
      when 'q'       => action := abort_now;
      when 'r'       => action := rename_it; Put ("New name: ");
                        Get_Line (new_name, new_name_length);
      when others    => null;
    end case;

    --  Cosmetic : position for the [.....]
    Put ("                                                                    ");
  end My_resolve_conflict;

  procedure My_get_password
   (password : out Ada.Strings.Unbounded.Unbounded_String)
  is
    c : Character;
    use Ada.Strings.Unbounded, Ada.Text_IO;
  begin
    New_Line;
    Put_Line (" Current password is incorrect.");
    Put (" Password please : ");
    --  Fake "Get_line( password );" without echo.
    --  We use Get_Immediate that has no echo on GNAT/Windows - no mention
    --  of that feature in the (A)RM95, so no warranty about it!

    password := To_Unbounded_String ("");

    loop
      Get_Immediate (c);
      exit when c = ASCII.CR;
      Put ('*');
      password := password & c;
    end loop;

    New_Line;

    --  Cosmetic : position for the [.....]
    Put ("                                                                    ");
  end My_get_password;

end Zip_Console_IO;
