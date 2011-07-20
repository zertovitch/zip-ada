------------------------------------------------------------------------------
--  File:            ZipAda.adb
--  Description:     A minimal standalone command-line zip archiving utility
--                     using the Zip-Ada library.
--  Date/version:    19-Oct-2009; ... ; Dec-2007
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
-- Important changes:
--
-- ZA v. 28: uses Zip.Create
-- ZA v. 26: modified for new Zip_Stream

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Streams;                       use Ada.Streams;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Characters.Handling;           use Ada.Characters.Handling;

with Zip_Streams;                       use Zip_Streams;
with Zip.Compress, Zip.Create;

with My_feedback;

procedure ZipAda is

  T0, T1 : Ada.Calendar.Time;
  seconds: Duration;

  procedure Blurb is
  begin
    Put_Line("ZipAda * minimalistic standalone zipping tool.");
    Put_Line("Demo for Zip-Ada library, by G. de Montmollin");
    Put_Line("Library version " & Zip.version & " dated " & Zip.reference );
    Put_Line("URL: " & Zip.web);
    New_Line;
  end Blurb;

  function CutName(n:String; l:Natural) return String is
    dots: constant String:= "...";
  begin
    if n'Length > l then
      return dots & n( n'Last - (l-1) + dots'Length .. n'Last );
    else
      return n;
    end if;
  end CutName;

  --  Final zipfile stream
  MyStream : aliased ZipFile_Stream;
  ZipFileStream : constant Zipstream_Class := MyStream'Unchecked_Access;
  Info: Zip.Create.Zip_Create_info;

  procedure Add_1_Stream(Stream : Zipstream_Class) is
    Compressed_Size: Zip.File_size_type;
    Final_Method   : Natural;
  begin
    Put("  Adding ");
    declare
      maxlen: constant:= 24;
      cut: constant String:= CutName( Get_Name(Stream), maxlen );
    begin
      Put( cut & (1 + maxlen - cut'Length) * ' ');
    end;
    --
    Zip.Create.Add_Stream(
      Info, Stream, My_feedback'Access, Compressed_Size, Final_Method
    );
    --
    if Size(Stream) = 0 then
      Put("          ");
    end if;
    Put(' ');
    declare
      meth: constant String:=
        To_Lower(Zip.PKZip_method'Image(
          Zip.Method_from_code(Final_Method)
        ));
    begin
      Put( meth & (Zip.PKZip_method'Width - meth'Length) * ' ');
    end;
    if Size(Stream) > 0 then
      Put(", to ");
      Put(100.0 * Float(Compressed_Size) / Float(Size(Stream)), 3,2,0 );
      Put('%');
    end if;
    Put_Line(", done.");
  end Add_1_Stream;

  function Add_zip_ext(s: String) return String is
  begin
    if s'Length < 4 or else
       To_Upper(s(s'Last-3..s'Last)) /= ".ZIP"
    then
      return s & ".zip";
    else
      return s;
    end if;
  end Add_zip_ext;

  use Zip.Compress;

  method: Compression_Method:= shrink;
  zip_name_set: Boolean:= False;

begin
  Blurb;
  for I in 1..Argument_Count loop
    declare
      arg    : constant String:= Argument(I);
      arg_zip: constant String:= Add_zip_ext(arg);
      answer : Character;
    begin
      if arg(1) = '-' or arg(1) = '/' then
        -- Options
        declare
          opt: constant String:= arg(arg'First+1..arg'Last) & ' ';
        begin
          if opt(opt'First..opt'First+1) = "er" then
            case opt(opt'First+2) is
              when '1'    => method:= reduce_1;
              when '2'    => method:= reduce_2;
              when '3'    => method:= reduce_3;
              when others => method:= reduce_4;
            end case;
          elsif opt(opt'First..opt'First+1) = "es" then
            method:= shrink;
          elsif opt(opt'First..opt'First+2) = "edf" then
            method:= deflate_fixed;
          end if;
        end;
      elsif not zip_name_set then
        zip_name_set:= True;
        if Zip.Exists(arg_zip) then
          Put("Archive " & arg_zip & " already exists! Overwrite (y/n) ?");
          Get_Immediate(answer);
          answer:= To_Upper(answer);
          Put_Line(" -> " & answer);
          if answer/='Y' then
            Put_Line("Stopped.");
            return;
          end if;
        end if;
        Put_Line("Creating archive " & arg_zip);
        T0:= Clock;
        Zip.Create.Create(Info, ZipFileStream, arg_zip, method);
      else -- First real argument has already been used for archive's name
        if To_Upper(arg) = To_Upper(Zip.Create.Name(Info)) then
          Put_Line("  ** Warning: skipping archive's name as entry: " & arg);
          -- avoid zipping the archive itself!
          -- NB: case insensitive
        elsif Zip.Exists(arg) then
          declare
            InStream   : aliased ZipFile_Stream;
            StreamFile : constant Zipstream_Class := InStream'Unchecked_Access;
          begin
            Set_Name (StreamFile, arg);
            Set_Time (StreamFile, Ada.Directories.Modification_Time(arg));
            Open (InStream, In_File);
            Add_1_Stream (StreamFile);
            Close (InStream);
          exception
            when Ada.Text_IO.Use_Error =>
              Put_Line("  ** Warning: skipping invalid entry: " & arg);
          end;
        else
          Put_Line("  ** Warning: name not matched: " & arg);
        end if;
      end if;
    end;
  end loop;
  if Argument_Count > 1 then
    Zip.Create.Finish (Info);
    T1:= Clock;
    seconds:= T1-T0;
    Put("Time elapsed : ");
    Put( Float( seconds ), 4, 2, 0 );
    Put_Line( " sec");
  else
    Put_Line("Usage: zipada [options] archive[.zip] file(s)");
    New_Line;
    Put_Line("options:  -erN   : use the 2-pass ""reduce"" method, factor N=1..4");
    Put_Line("          -es    : ""shrink"" (LZW algorithm)");
    Put_Line("          -edf   : ""deflate"", with one fixed block");
  end if;
end ZipAda;
