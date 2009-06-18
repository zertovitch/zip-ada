with Ada.Streams; use Ada.Streams;
package body Zip.Headers is

  -----------------------------------------------------------
  -- Byte array <-> various integers, with Intel endianess --
  -----------------------------------------------------------

  -- Get numbers with correct trucmuche endian, to ensure
  -- correct header loading on some non-Intel machines

  generic
    type Number is mod <>; -- range <> in Ada83 version (fake Interfaces)
  function Intel_x86_number( b: Byte_Buffer ) return Number;

  function Intel_x86_number( b: Byte_Buffer ) return Number is
    n: Number:= 0;
  begin
    for i in reverse b'Range loop
      n:= n * 256 + Number(b(i));
    end loop;
    return n;
  end Intel_x86_number;

  function Intel_nb is new Intel_x86_number( Unsigned_16 );
  function Intel_nb is new Intel_x86_number( Unsigned_32 );

  -- Put numbers with correct endianess as bytes

  generic
    type Number is mod <>; -- range <> in Ada83 version (fake Interfaces)
    size: Positive;
  function Intel_x86_buffer( n: Number ) return Byte_Buffer;

  function Intel_x86_buffer( n: Number ) return Byte_Buffer is
    b: Byte_Buffer(1..size);
    m: Number:= n;
  begin
    for i in b'Range loop
      b(i):= Unsigned_8(m and 255);
      m:= m / 256;
    end loop;
    return b;
  end Intel_x86_buffer;

  function Intel_bf is new Intel_x86_buffer( Unsigned_16, 2 );
  function Intel_bf is new Intel_x86_buffer( Unsigned_32, 4 );

  -------------------
  -- PK signatures --
  -------------------

  function PK_signature( buf: Byte_Buffer; code: Unsigned_8 ) return Boolean is
  begin
    return buf(1..4) = (16#50#, 16#4B#, code, code+1); -- PK12, PK34, ...
  end PK_signature;

  procedure PK_signature( buf: in out Byte_Buffer; code: Unsigned_8 ) is
  begin
    buf(1..4) := (16#50#, 16#4B#, code, code+1); -- PK12, PK34, ...
  end PK_signature;


  -----------------------------------------------------
  -- DOS Time. Valid through Year 2107, but          --
  -- still better than Ada95's Ada.Calendar's 2099 ! --
  -----------------------------------------------------

  function DOS_Time(date : in Time) return Unsigned_32 is
    year            : Year_Number;
    month           : Month_Number;
    day             : Day_Number;
    seconds_day_dur : Day_Duration;
    seconds_day     : Unsigned_32;
    hours           : Unsigned_32;
    minutes         : Unsigned_32;
    seconds         : Unsigned_32;
  begin
    Split(date, year, month, day, seconds_day_dur);
    if year < 1980 then -- avoid invalid DOS date
      year:= 1980;
    end if;
    seconds_day:= Unsigned_32(seconds_day_dur);
    hours:= seconds_day / 3600;
    minutes:=  (seconds_day / 60) mod 60;
    seconds:= seconds_day mod 60;
    return
      -- MSDN formula:
        Unsigned_32( (year - 1980) * 512 + month * 32 + day ) * 65536 -- Date
      +
        hours * 2048 + minutes * 32 + seconds/2; -- Time
  end DOS_Time;

  function Ada_Time(DOS_date: Unsigned_32) return Time is
    d_date : constant Integer:= Integer(DOS_date  /  65536);
    d_time : constant Integer:= Integer(DOS_date and 65535);
    year   : Year_Number;
    month  : Month_Number;
    day    : Day_Number;
    hours  : Integer;
    minutes: Integer;
    seconds: Integer;
  begin
    year := 1980 + d_date / 512;
    month:= (d_date / 32) mod 16;
    day  := d_date mod 32;
    hours   := d_time / 2048;
    minutes := (d_time / 32) mod 64;
    seconds := 2 * (d_time mod 32);
    return Time_Of(
      year, month, day,
      Day_Duration(hours * 3600 + minutes * 60 + seconds)
    );
  end Ada_Time;

  -------------------------------------------------------
  -- PKZIP file header, as in central directory - PK12 --
  -------------------------------------------------------
  procedure Read_and_check(
    stream : in  Zipstream_Class;
    header : out Central_File_Header
  )
  is
    chb: Byte_Buffer( 1..46 );
  begin
    Byte_Buffer'Read(stream, chb);

    if not PK_signature(chb, 1) then
      raise bad_central_header;
    end if;

    header.made_by_version:=                   Intel_nb( chb( 5.. 6) );
    header.short_info.needed_extract_version:= Intel_nb( chb( 7.. 8) );
    header.short_info.bit_flag:=               Intel_nb( chb( 9..10) );
    header.short_info.zip_type:=               Intel_nb( chb(11..12) );
    header.short_info.file_timedate:= Ada_Time(Intel_nb( chb(13..16) ));
    header.short_info.dd.crc_32:=              Intel_nb( chb(17..20) );
    header.short_info.dd.compressed_size:=     Intel_nb( chb(21..24) );
    header.short_info.dd.uncompressed_size:=   Intel_nb( chb(25..28) );
    header.short_info.filename_length:=        Intel_nb( chb(29..30) );
    header.short_info.extra_field_length:=     Intel_nb( chb(31..32) );
    header.comment_length:=                    Intel_nb( chb(33..34) );
    header.disk_number_start:=                 Intel_nb( chb(35..36) );
    header.internal_attributes:=               Intel_nb( chb(37..38) );
    header.external_attributes:=               Intel_nb( chb(39..42) );
    header.local_header_offset:=               Intel_nb( chb(43..46) );

  end Read_and_check;

  procedure Write(
    stream : in     Zipstream_Class;
    header : in     Central_File_Header
  )
  is
    chb: Byte_Buffer( 1..46 );
  begin
    PK_signature(chb, 1);

    chb( 5.. 6):= Intel_bf( header.made_by_version );
    chb( 7.. 8):= Intel_bf( header.short_info.needed_extract_version );
    chb( 9..10):= Intel_bf( header.short_info.bit_flag );
    chb(11..12):= Intel_bf( header.short_info.zip_type );
    chb(13..16):= Intel_bf( DOS_Time(header.short_info.file_timedate) );
    chb(17..20):= Intel_bf( header.short_info.dd.crc_32 );
    chb(21..24):= Intel_bf( header.short_info.dd.compressed_size );
    chb(25..28):= Intel_bf( header.short_info.dd.uncompressed_size );
    chb(29..30):= Intel_bf( header.short_info.filename_length );
    chb(31..32):= Intel_bf( header.short_info.extra_field_length );
    chb(33..34):= Intel_bf( header.comment_length );
    chb(35..36):= Intel_bf( header.disk_number_start );
    chb(37..38):= Intel_bf( header.internal_attributes );
    chb(39..42):= Intel_bf( header.external_attributes );
    chb(43..46):= Intel_bf( header.local_header_offset );

    Byte_Buffer'Write(stream, chb);
  end Write;

  -----------------------------------------------------------------------
  -- PKZIP local file header, in front of every file in archive - PK34 --
  -----------------------------------------------------------------------
  procedure Read_and_check(
    stream : in     Zipstream_Class;
    header :    out Local_File_Header
  )
  is
    lhb: Byte_Buffer( 1..30 );
  begin
    Byte_Buffer'Read(stream, lhb);

    if not PK_signature(lhb, 3) then
      raise bad_local_header;
    end if;

    header.needed_extract_version:= Intel_nb( lhb( 5.. 6) );
    header.bit_flag:=               Intel_nb( lhb( 7.. 8) );
    header.zip_type:=               Intel_nb( lhb( 9..10) );
    header.file_timedate:= Ada_Time(Intel_nb( lhb(11..14) ));
    header.dd.crc_32:=              Intel_nb( lhb(15..18) );
    header.dd.compressed_size:=     Intel_nb( lhb(19..22) );
    header.dd.uncompressed_size:=   Intel_nb( lhb(23..26) );
    header.filename_length:=        Intel_nb( lhb(27..28) );
    header.extra_field_length:=     Intel_nb( lhb(29..30) );

  end Read_and_check;

  procedure Write(
                  stream : in     Zipstream_Class;
                  header : in     Local_File_Header
                 )
  is
    lhb: Byte_Buffer( 1..30 );
  begin
    PK_signature(lhb, 3);

    lhb( 5.. 6):= Intel_bf( header.needed_extract_version );
    lhb( 7.. 8):= Intel_bf( header.bit_flag );
    lhb( 9..10):= Intel_bf( header.zip_type );
    lhb(11..14):= Intel_bf( DOS_Time(header.file_timedate) );
    lhb(15..18):= Intel_bf( header.dd.crc_32 );
    lhb(19..22):= Intel_bf( header.dd.compressed_size );
    lhb(23..26):= Intel_bf( header.dd.uncompressed_size );
    lhb(27..28):= Intel_bf( header.filename_length );
    lhb(29..30):= Intel_bf( header.extra_field_length );

    Byte_Buffer'Write(stream, lhb);
  end Write;

  -------------------------------------------
  -- PKZIP end-of-central-directory - PK56 --
  -------------------------------------------
  procedure Copy_and_check(
    buffer  : in     Byte_Buffer;
    the_end :    out End_of_Central_Dir
  )
  is
  begin
    if not PK_signature(buffer, 5) then
      raise bad_end;
    end if;

    the_end.disknum:=              Intel_nb( buffer( 5.. 6) );
    the_end.disknum_with_start:=   Intel_nb( buffer( 7.. 8) );
    the_end.disk_total_entries:=   Intel_nb( buffer( 9..10) );
    the_end.total_entries:=        Intel_nb( buffer(11..12) );
    the_end.central_dir_size:=     Intel_nb( buffer(13..16) );
    the_end.central_dir_offset:=   Intel_nb( buffer(17..20) );
    the_end.main_comment_length:=  Intel_nb( buffer(21..22) );

  end Copy_and_check;

  procedure Read_and_check(
    stream  : in     Zipstream_Class;
    the_end :    out End_of_Central_Dir
  )
  is
    eb: Byte_Buffer( 1..22 );
  begin
    Byte_Buffer'Read(stream, eb);
    Copy_and_check(eb, the_end);
  end Read_and_check;

  -- Some explanations - GdM 2001

  -- The idea is that the .ZIP can be appended to an .EXE, for
  -- self-extracting purposes. So, the most general infos are
  -- at the end, and we crawl back for more precise infos:
  --  1) end-of-central directory
  --  2) central directory
  --  3) zipped files

  procedure Load(
    stream : in     Zipstream_Class;
    the_end:    out End_of_Central_Dir
    )
  is
    end_buffer: Byte_Buffer( 1..22 );
    min_end_start: Ada.Streams.Stream_IO.Count;
    use Ada.Streams.Stream_IO;
    max_comment: constant:= 65_535;
  begin
    -- 20-Jun-2001: abandon search below min_end_start
    --              - read about max comment length in appnote

    if Size(stream) <= max_comment then
      min_end_start:= 1;
    else
      min_end_start:= Ada.Streams.Stream_IO.Count(Size(stream)) - max_comment;
    end if;

    -- Yes, we must _search_ for it...
    -- because PKWARE put a variable-size comment _after_ it 8-(

    for i in reverse min_end_start .. Ada.Streams.Stream_IO.Count(Size(stream)) - 21 loop
      Zip_Streams.Set_Index(stream, Positive(i));
      begin
        for j in end_buffer'Range loop
          Byte'Read(stream, end_buffer(j));
          -- 20-Jun-2001: useless to read more if 1st character is not 'P'
          if j=end_buffer'First and then
             end_buffer(j)/=Character'Pos('P') then
            raise bad_end;
          end if;
        end loop;
        Copy_and_check( end_buffer, the_end );
        return; -- the_end found and filled -> exit
      exception
        when bad_end =>
          if i > min_end_start then
            null;  -- we will try 1 index before...
          else
            raise; -- definitely no "end-of-central-directory" here
          end if;
      end;
    end loop;
  end Load;

  procedure Write(
    stream  : in     Zipstream_Class;
    the_end : in     End_of_Central_Dir
  )
  is
    eb: Byte_Buffer( 1..22 );
  begin
    PK_signature(eb, 5);

    eb( 5.. 6):= Intel_bf( the_end.disknum );
    eb( 7.. 8):= Intel_bf( the_end.disknum_with_start );
    eb( 9..10):= Intel_bf( the_end.disk_total_entries );
    eb(11..12):= Intel_bf( the_end.total_entries );
    eb(13..16):= Intel_bf( the_end.central_dir_size );
    eb(17..20):= Intel_bf( the_end.central_dir_offset );
    eb(21..22):= Intel_bf( the_end.main_comment_length );

    Byte_Buffer'Write(stream, eb);
  end Write;

  ------------------------------------------------------------------
  -- PKZIP data descriptor, after streamed compressed data - PK78 --
  ------------------------------------------------------------------
  procedure Copy_and_check(
    buffer        : in     Byte_Buffer;
    the_data_desc :    out Data_descriptor
  )
  is
  begin
    if not PK_signature(buffer, 7) then
      raise bad_data_descriptor;
    end if;

    the_data_desc.crc_32:=             Intel_nb( buffer(5..8) );
    the_data_desc.compressed_size:=    Intel_nb( buffer(9..12) );
    the_data_desc.uncompressed_size:=  Intel_nb( buffer(13..16) );

  end Copy_and_check;

  procedure Read_and_check(
    stream        : in     Zipstream_Class;
    the_data_desc :    out Data_descriptor
  )
  is
    ddb: Byte_Buffer( 1..16 );
  begin
    Byte_Buffer'Read(stream, ddb);
    Copy_and_check(ddb, the_data_desc);
  end Read_and_check;

  procedure Write(
    stream        : in Zipstream_Class;
    the_data_desc : in Data_descriptor
  )
  is
    ddb: Byte_Buffer( 1..16 );
  begin
    PK_signature(ddb, 7);

    ddb( 5.. 8):= Intel_bf( the_data_desc.crc_32 );
    ddb( 9..12):= Intel_bf( the_data_desc.compressed_size );
    ddb(13..16):= Intel_bf( the_data_desc.uncompressed_size );

    Byte_Buffer'Write(stream, ddb);
  end Write;

end Zip.Headers;
