-- Draft of a zip archiving utility.
--
-- UZA v. 28: uses Zip.Create
-- UZA v. 26: modified for new Zip_Stream

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

  T0, T1 : Time;
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
      cut: constant String:= Cutname( GetName(Stream), maxlen );
    begin
      Put( cut & (1 + maxlen - cut'Length) * ' ');
    end;
    --
    Zip.Create.Add_Stream(
      Info, Stream, My_Feedback'Access, Compressed_Size, Final_Method
    );
    --
    if Size(Stream) = 0 then
      Put("          ");
    end if;
    Put(' ');
    declare
      meth: constant String:=
        To_Lower(Zip.pkzip_method'Image(
          Zip.Method_from_code(Final_Method)
        ));
    begin
      Put( meth & (Zip.pkzip_method'Width - meth'Length) * ' ');
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

  zip_name_set: Boolean:= False;
  answer: Character;
  InStream : array(2..Argument_Count) of aliased ZipFile_Stream;

  use Zip.Compress;

  method: Compression_Method:= shrink;

begin
  Blurb;
  for I in 1..Argument_Count loop
    declare
      arg    : constant String:= Argument(I);
      arg_zip: constant String:= Add_zip_ext(arg);
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
        T0:= Clock;
        Zip.Create.Create(Info, ZipFileStream, arg_zip, method);
      else -- First real argument already used for archive's name
        if Zip.Exists(arg) then
          declare
            StreamFile : constant Zipstream_Class := InStream (I)'Unchecked_Access;
          begin
            SetName (StreamFile, arg);
            SetTime (StreamFile, Ada.Directories.Modification_Time(arg));
            Open (ZipFile_Stream (StreamFile.all), In_File);
            Add_1_Stream (StreamFile);
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
  end if;
end ZipAda;
