------------------------------------------------------------------------------
--  File:            Summary.ads
--  Description:     Part of the UnZipAda demo
------------------------------------------------------------------------------
with UnZip;

package Summary is

  total_uncompressed, total_compressed : UnZip.File_size_type;
  total_entries : Natural;
  files_per_method : array (UnZip.PKZip_method) of Natural;
  uncompressed_per_method,
  compressed_per_method : array (UnZip.PKZip_method) of UnZip.File_size_type;

  procedure Reset;

  function Nice_image (format : UnZip.PKZip_method) return String;

end Summary;
