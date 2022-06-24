--  Console I/O for ZipAda, UnZipAda and ReZip tools.
--  It's not nice code (global variables), so please don't use it elsewhere.

with UnZip;
with Zip;

with Ada.Strings.Unbounded;

package Zip_Console_IO is

  package Summary is
    total_uncompressed, total_compressed : Zip.Zip_64_Data_Size_Type;
    total_entries                        : Natural;
    files_per_method                     : array (Zip.PKZip_method) of Natural;
    uncompressed_per_method,
    compressed_per_method                : array (Zip.PKZip_method) of Zip.Zip_64_Data_Size_Type;
    --
    procedure Reset;
    function Nice_image (format : Zip.PKZip_method) return String;
  end Summary;

  procedure My_feedback
   (percents_done :  in Natural;
    entry_skipped :  in Boolean;
    user_abort    : out Boolean);

  procedure My_tell_data
   (file_name          : String;
    compressed_bytes   : Zip.Zip_64_Data_Size_Type;
    uncompressed_bytes : Zip.Zip_64_Data_Size_Type;
    method             : Zip.PKZip_method);

  procedure My_resolve_conflict
   (file_name       :  in String;
    name_encoding   :  in Zip.Zip_name_encoding;
    action          : out UnZip.Name_conflict_intervention;
    new_name        : out String;
    new_name_length : out Natural);

  procedure My_get_password
   (password : out Ada.Strings.Unbounded.Unbounded_String);

end Zip_Console_IO;
