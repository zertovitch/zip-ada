------------------------------------------------------------------------------
--  File:            rezip_lib.ads
--  Description:     Recompression tool to make archives smaller.
--                   Core moved from Rezip (main)
--                     Uses brute force and pick-and-choose among compression
--                     tools and methods. Typically the optimal archive will
--                     contain some entries compressed with the LZMA format,
--                     and others with the Deflate, Deflate_e or BZip2.
--                     Compression speed doesn't matter (except extreme cases),
--                     only the final size does.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip;

package Rezip_lib is

  type Zip_format_set is private;

  all_formats        : constant Zip_format_set;
  deflate_or_store   : constant Zip_format_set;
  fast_decompression : constant Zip_format_set;

  procedure Rezip (
    from_zip_file      : String;
    to_zip_file        : String;
    format_choice      : Zip_format_set := all_formats;  --  force output into selected format set
    touch              : Boolean        := False;        --  set time stamps to now
    lower              : Boolean        := False;        --  set full file names to lower case
    delete_comment     : Boolean        := False;        --  delete zip comment
    randomized_stable  : Positive       := 1;
    log_file           : String         := "";
    html_report        : String         := "";
    alt_tmp_file_radix : String         := "";           --  e.g. "X:\temp\rz_"
    internal_only      : Boolean        := False         --  Zip-Ada algorithms only, no ext. call
  );
    --  On randomized approaches, stop when minimized size is stable
    --  after randomized_stable attempts.

  procedure Show_external_packer_list;

  External_Tool_Failed : exception;

private

  type Zip_format_set is array (Zip.PKZip_method) of Boolean;

  all_formats        : constant Zip_format_set := (others => True);
  deflate_or_store   : constant Zip_format_set :=
    (Zip.store | Zip.deflate => True, others => False);
  fast_decompression : constant Zip_format_set :=
    (Zip.store .. Zip.deflate_e | Zip.lzma_meth => True,
     Zip.bzip2_meth => False,
     others => False);

end Rezip_lib;
