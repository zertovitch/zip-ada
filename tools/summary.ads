------------------------------------------------------------------------------
--  File:            Summary.ads
--  Description:     Part of the UnZipAda demo
------------------------------------------------------------------------------

with Zip;

package Summary is

  total_uncompressed, total_compressed : Zip.Zip_64_Data_Size_Type;
  total_entries : Natural;
  files_per_method : array (Zip.PKZip_method) of Natural;
  uncompressed_per_method,
  compressed_per_method : array (Zip.PKZip_method) of Zip.Zip_64_Data_Size_Type;

  procedure Reset;

  function Nice_image (format : Zip.PKZip_method) return String;

end Summary;
