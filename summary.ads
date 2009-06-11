------------------------------------------------------------------------------
--  File:            Summary.ads
--  Description:     part of Unzipada demo
------------------------------------------------------------------------------
with Unzip;

package Summary is
  total_uncompressed, total_compressed: Unzip.File_size_type;
  Total_Entries: Natural;
  procedure Reset;
end Summary;