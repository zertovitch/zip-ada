------------------------------------------------------------------------------
--  File:            Comp_Zip.adb
--  Description:     A zip comparison tool using Zip-Ada lib.
--                   Demonstrates the new Zip.Traverse procedure
--  Date/version:    9-Mar-2007 ; 31-Oct-2006 ; 4-Jul-2004 ; 29-Nov-2002
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Characters.Handling;           use Ada.Characters.Handling;

with Zip;
with UnZip.Streams;                     use UnZip.Streams;

procedure Comp_Zip is
  z: array(1..2) of Zip.Zip_info;
  total_1,
  total_2,
  common,
  size_failures,
  compare_failures,
  missing_1_in_2,
  just_a_directory,
  missing_2_in_1,
  total_errors: Natural:= 0;
  total_bytes: Long_Integer:= 0;

  procedure Compare_1_file( name: String ) is
    f: array(1..2) of Zipped_File_Type;
    s: array(1..2) of Stream_Access;
    c: array(1..2) of Character;
    p: Long_Integer:= 1;

    function CutName(n:String; l:Natural) return String is
      dots: constant String:= "...";
    begin
      if n'Length > l then
        return dots & n( n'Last - (l-1) + dots'Length .. n'Last );
      else
        return n;
      end if;
    end CutName;

    l: constant:= 20;
    mininame: constant String:= To_Lower(CutName(name,l));
    stuffing: constant String(1..l-mininame'Length+1):= (others => ' ');

  begin
    Put("   [" & stuffing & mininame & "] ");
    for i in 1..2 loop
      begin
        Open( f(i), z(i), name );
        if i=1 then
          total_1:= total_1 + 1;
        end if;
      exception
        when Zip.File_name_not_found =>
          Put( "   # Not found in archive [" & Argument(i) & ']' );
          if i = 1 then
            Put_Line("-- internal error!");
          else
            Close(f(1));
          end if;
          if name(name'Last)='/' or name(name'Last)='\' then
            just_a_directory:= just_a_directory + 1;
            Put_Line(" (just a dir.)");
          else
            New_Line;
          end if;
          missing_1_in_2:= missing_1_in_2 + 1;
          return;
      end;
      s(i):= Stream(f(i));
    end loop;
    -- File found, now the comparison:
    while not End_of_file(f(1)) loop
      if End_of_file(f(2)) then
        Put_Line("   # Shorter in [" & Argument(2) & "] at position" &
                 Long_Integer'Image(p) );
        Close(f(1));
        Close(f(2));
        size_failures:= size_failures + 1;
        return;
      end if;

      for i in 1..2 loop
        Character'Read(s(i),c(i));
      end loop;
      if c(1)/=c(2) then
        Put_Line("   # Difference at position" & Long_Integer'Image(p) );
        Close(f(1));
        Close(f(2));
        compare_failures:= compare_failures + 1;
        return;
      end if;
      p:= p+1;
    end loop;
    if not End_of_file(f(2)) then
      Put_Line("   # Longer in [" & Argument(2) & "]" );
      Close(f(1));
      Close(f(2));
      size_failures:= size_failures + 1;
      return;
    end if;
    Close(f(1));
    Close(f(2));
    Put_Line("OK -" & Long_Integer'Image(p-1) & " bytes compared");
    total_bytes:= total_bytes + (p-1);
  end Compare_1_file;

  procedure Compare_all_files is new Zip.Traverse( Compare_1_file );

  err_str: String(1..5);

  function Exist(name:String) return Boolean is
    f: File_Type;
  begin
    Open(f,in_file,name);
    Close(f);
    return True;
  exception
    when Name_Error => return False;
  end Exist;

  function Try_with_zip(name: String) return String is
  begin
    if Exist(name) then
      return name;
    else
      return name & ".zip";
      -- Maybe the file doesn't exist, but we tried our best...
    end if;
  end Try_with_zip;

begin
  if Argument_Count < 2 then
    Put_Line("Comp_Zip * compare two zip archive files, incl. contents");
    Put_Line("Demo for the Zip-Ada library, by G. de Montmollin");
    Put_Line("Library version " & Zip.version & " dated " & Zip.reference );
    Put_Line("URL: " & Zip.web);
    New_Line;
    Put_Line("Usage: comp_zip archive1[.zip] archive2[.zip]");
    return;
  end if;
  for i in 1..2 loop
    declare
      n: constant String:= Try_with_zip(Argument(i));
    begin
      Zip.Load( z(i), n );
    exception
      when Zip.Zip_file_open_error =>
        Put( "Can't open archive [" & n & ']' ); raise;
      when UnZip.Wrong_password      =>
        Put( "Archive has a password" ); raise;
    end;
  end loop;
  Put_Line("* Comparing [" & Argument(1) & "] and [" & Argument(2) & "] :");
  Compare_all_files(z(1));
  total_2:= Zip.Entries(z(2));
  common:= total_1 - missing_1_in_2;
  Put_Line("* === Results ===");
  Put_Line("  1st archive: [" & Argument(1) & "], total files:" & Natural'Image(total_1));
  Put_Line("  2nd archive: [" & Argument(2) & "], total files:" & Natural'Image(total_2));
  Put_Line("  Total files compared: " & Natural'Image(common));
  Put_Line("  Total of correct bytes: " & Long_Integer'Image(total_bytes));
  Put_Line("* === Error summary ===");
  Put(err_str,size_failures);
  Put_Line("    Size failures . . . . . . . . . . . :" & err_str);
  Put(err_str,compare_failures);
  Put_Line("    Content comparison failures . . . . :" & err_str);
  Put(err_str,missing_1_in_2);
  Put("    Files of 1st archive missing in 2nd :" & err_str);

  if just_a_directory > 0 then
    Put_Line(" (" & Integer'Image(just_a_directory) & " useless dir. names)");
  else
    New_Line;
  end if;

  missing_2_in_1:= total_2 - common;
  -- t2 - m21 = t1 - m12 = # common files
  Put(err_str,missing_2_in_1);
  for i in err_str'Range loop
    if err_str(i)=' ' then err_str(i):='_'; end if;
  end loop;
  Put_Line("  __Files of 2nd archive missing in 1st :" & err_str & "__");

  total_errors:=
     size_failures + compare_failures +
     missing_1_in_2 + missing_2_in_1;

  Put(err_str,total_errors);
  Put_Line("  Total of errors . . . . . . . . . . . :" & err_str);
end Comp_Zip;
