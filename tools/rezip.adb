------------------------------------------------------------------------------
--  File:            rezip.adb
--  Description:     Recompression tool to make archives smaller.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Rezip_lib, Comp_Zip_Prc, Zip;
with Show_License;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;
with Ada.Characters.Handling;           use Ada.Characters.Handling;

procedure ReZip is

  procedure Blurb is
    use Ada.Text_IO;
  begin
    Put_Line ("ReZip * Zip file recompression tool.");
    Put_Line ("Author: Gautier de Montmollin");
    Put_Line ("Library version " & Zip.version & " dated " & Zip.reference);
    Put_Line ("URL: " & Zip.web);
    Show_License (Current_Output, "zip.ads");
  end Blurb;

  procedure Usage is
  begin
    Ada.Text_IO.Put_Line ("Usage: rezip [options] archive(s)[.zip]");
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line ("Options:  -defl     : repack archive only with the Deflate");
    Ada.Text_IO.Put_Line ("                        subformat (most compatible)");
    Ada.Text_IO.Put_Line ("          -fast_dec : repack archive only with fast decompressing subformats");
    Ada.Text_IO.Put_Line ("          -int      : use internal Zip-Ada algorithms only, no external call");
    Ada.Text_IO.Put_Line ("          -touch    : set time stamps to now");
    Ada.Text_IO.Put_Line ("          -lower    : set full file names to lower case");
    Ada.Text_IO.Put_Line ("          -del_comm : delete comment");
    Ada.Text_IO.Put_Line ("          -comp     : compare original and repacked archives (paranoid mode)");
    Ada.Text_IO.Put_Line ("          -rs=n     : loop many times over a single compression approach");
    Ada.Text_IO.Put_Line ("                        having randomization, and keep optimum when size is");
    Ada.Text_IO.Put_Line ("                        stable after n attempts in a row");
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line ("External programs (available for Windows and Linux) are used, except");
    Ada.Text_IO.Put_Line ("with the ""-int"" option. They must be callable through the ""path"".");
    Ada.Text_IO.Put_Line ("List of external programs:");
    Ada.Text_IO.New_Line;
    Rezip_lib.Show_external_packer_list;
    Ada.Text_IO.New_Line;
  end Usage;

  function Add_zip_ext (s : String) return String is
  begin
    if Zip.Exists (s) then
      return s;
    else
      return s & ".zip";
      --  Maybe the file doesn't exist, but we tried our best...
    end if;
  end Add_zip_ext;

  function Get_ext (s : String) return String is
    dot : Integer := s'Last;
  begin
    for i in reverse s'Range loop
      if s (i) = '.' then
        dot := i;
        exit;
      end if;
    end loop;
    if s = "" or dot = s'Last then -- no extension in all cases:
      return "zip";              -- "", "xxx." or "xxx"
    else
      return s (dot + 1 .. s'Last);
    end if;
  end Get_ext;

  function Remove_ext (s : String) return String is
    dot : Integer := s'Last + 1;
  begin
    if s = "" then
      return s;
    end if;
    for i in reverse s'Range loop
      if s (i) = '.' then
        dot := i;
        exit;
      end if;
    end loop;
    return s (s'First .. dot - 1);
    --  "xxx" returned in all cases: "xxx.ext", "xxx." or "xxx"
  end Remove_ext;

  use Rezip_lib;

  touch, lower, del_comment, compare, internal : Boolean := False;
  rand_stable : Positive := 1;
  format_choice : Zip_format_set := all_formats;

begin
  Blurb;
  if Argument_Count = 0 then
    Usage;
    return;
  end if;
  for i in 1 .. Argument_Count loop
    declare
      arg       : constant String := Argument (i);
      arg_zip   : constant String := Add_zip_ext (arg);
      ext       : constant String := Get_ext (arg_zip);
      arg_nozip : constant String := Remove_ext (arg_zip);
      arg_rezip : constant String := arg_nozip & ".repacked." & ext;
      arg_rpt   : constant String := arg_nozip & ".ReZip.html";
      arg_log   : constant String := arg_nozip & ".ReZip.log";
      info_original_zip,
      info_rezipped_zip : Zip.Zip_info;
    begin
      if arg (arg'First) = '-' or arg (arg'First) = '/' then
        --  Options
        declare
          opt : constant String := To_Lower (arg (arg'First + 1 .. arg'Last));
        begin
          if opt = "defl" then
            format_choice := deflate_or_store;
          elsif opt = "fast_dec" then
            format_choice := fast_decompression;
          elsif opt = "int" then
            internal := True;
          elsif opt = "comp" then
            compare := True;
          elsif opt = "touch" then
            touch := True;
          elsif opt = "lower" then
            lower := True;
          elsif opt = "del_comm" then
            del_comment := True;
          elsif opt'Length > 12 and then
             opt (opt'First .. opt'First + 11) = "rand_stable="  --  old / long version of this option
          then
            rand_stable := Integer'Value (opt (opt'First + 12 .. opt'Last));
          elsif opt'Length > 3 and then
             opt (opt'First .. opt'First + 2) = "rs="
          then
            rand_stable := Integer'Value (opt (opt'First + 3 .. opt'Last));
          end if;
        end;
      elsif Zip.Exists (arg_zip) then
        Rezip_lib.Rezip (
          from_zip_file     => arg_zip,
          to_zip_file       => arg_rezip,
          format_choice     => format_choice,
          touch             => touch,
          lower             => lower,
          delete_comment    => del_comment,
          randomized_stable => rand_stable,
          log_file          => arg_log,
          html_report       => arg_rpt,
          internal_only     => internal
        );
        if compare then
          Zip.Load (info_original_zip, arg_zip);
          Zip.Load (info_rezipped_zip, arg_rezip);
          Comp_Zip_Prc (info_original_zip, info_rezipped_zip, 0);
        end if;
      else
        Ada.Text_IO.Put_Line ("  ** Error: archive not found: " & arg_zip);
      end if;
    end;
  end loop;
end ReZip;
