------------------------------------------------------------------------------
--  File:            Summary.ads
--  Description:     Part of UnZipAda demo
------------------------------------------------------------------------------
with UnZip;

package Summary is

  total_uncompressed, total_compressed: UnZip.File_size_type;
  total_entries: Natural;
  format_used: array(UnZip.PKZip_method) of Natural;

  procedure Reset;

  function Nice_image(format: UnZip.PKZip_method) return String;

end Summary;
