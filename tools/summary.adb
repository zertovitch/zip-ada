------------------------------------------------------------------------------
--  File:            Summary.adb
--  Description:     Part of the UnZipAda demo
------------------------------------------------------------------------------

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

  function Nice_image (format : UnZip.PKZip_method) return String is
    img_stuffed : String (1 .. UnZip.PKZip_method'Width) := (others => ' ');
    img : constant String := Zip.Image (format);
  begin
    img_stuffed (1 .. img'Length) := img;
    return img_stuffed;
  end Nice_image;

end Summary;
