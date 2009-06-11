------------------------------------------------------------------------------
--  File:            Summary.adb
--  Description:     part of Unzipada demo
------------------------------------------------------------------------------
package body Summary is

  procedure Reset is
  begin
    total_uncompressed:= 0;
    total_compressed  := 0;
    Total_Entries     := 0;
  end Reset;

end Summary;

