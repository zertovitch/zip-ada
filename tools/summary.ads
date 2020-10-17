------------------------------------------------------------------------------
--  File:            Summary.ads
--  Description:     Part of the UnZipAda demo
------------------------------------------------------------------------------

with Zip, UnZip;

package Summary is

  total_uncompressed, total_compressed : Zip.Zip_32_Data_Size_Type;
  total_entries : Natural;
  files_per_method : array (UnZip.PKZip_method) of Natural;
  uncompressed_per_method,
  compressed_per_method : array (UnZip.PKZip_method) of Zip.Zip_32_Data_Size_Type;

  procedure Reset;

  function Nice_image (format : UnZip.PKZip_method) return String;

end Summary;
