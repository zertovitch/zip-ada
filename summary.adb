------------------------------------------------------------------------------
--  File:            Summary.adb
--  Description:     Part of UnZipAda demo
------------------------------------------------------------------------------

with Ada.Characters.Handling;           use Ada.Characters.Handling;

package body Summary is

  procedure Reset is
  begin
    total_uncompressed:= 0;
    total_compressed  := 0;
    total_entries     := 0;
    format_used       := (others => 0);
  end Reset;

  function Nice_image(format: UnZip.PKZip_method) return String is
    img_stuffed: String(1..UnZip.PKZip_method'Width):= (others=> ' ');
    img: constant String:= UnZip.PKZip_method'Image(format);
  begin
    img_stuffed(1..img'Length):= To_Lower(img);
    return img_stuffed;
  end Nice_image;

end Summary;

