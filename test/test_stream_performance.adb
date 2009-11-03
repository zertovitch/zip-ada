-- Usage: test_stream_performance <big_file>
-- Produces .tmp files that are copies of <big_file>.
--
-- Example of output with GNAT GPL 2008 / Win32:
--
--  xxx'Write / xxx'Read (Stream attributes)......... 9.282530042 seconds
--  Workarounds with Stream_Element_Array buffer:
--    copy........................................... 0.444120412 seconds
--    overlay (read), unchecked_conversion (write)... 0.156874407 seconds
--    overlay........................................ 0.150155676 seconds
--  Factor (Copy)    20.900930898
--  Factor (Overlay) 61.819374993

--  Buffer size in bits..... 8192
--  SE Buffer size in bits.. 8192

--  File size in megabytes..... 2.46367E+01

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Interfaces;                        use Interfaces;

procedure Test_Stream_Performance is

  f_in, f_out: Ada.Streams.Stream_IO.File_Type;

  buffer_size, SE_buffer_size: Natural:= 0;
  -- To check if buffers are binary compatible (same packing)

  type Buffer is array(Natural range <>) of Unsigned_8;

  ------------------------------------------------
  -- 1) Stream attributes - xxx'Read, xxx'Write --
  ------------------------------------------------

  -- NB: usually we would just have: Buffer'Read(Stream(f_in), b);
  -- Here we care about end of file.
  --
  procedure Read_Attribute( b: out Buffer; last_read: out Natural ) is
    idx: constant Positive_Count:= Index(f_in);
    siz: constant Positive_Count:= Size(f_in);
  begin
    if End_Of_File(f_in) then
      last_read:= b'First-1;
    else
      last_read:= Natural'Min(b'First+Natural(siz-idx),b'Last);
      Buffer'Read(Stream(f_in), b(b'First .. last_read));
    end if;
  end Read_Attribute;

  procedure Write_Attribute( b: in Buffer ) is
  begin
    if buffer_size = 0 then
      buffer_size:= b'size; -- just for stats
    end if;
    Buffer'Write(Stream(f_out), b);
  end Write_Attribute;

  ---------------------------------------------
  -- 2) The Stream_Element_Array workarounds --
  ---------------------------------------------

  procedure Read_SE_Copy( b: out Buffer; last_read: out Natural ) is
    use Ada.Streams;
    First     : constant Stream_Element_Offset:= Stream_Element_Offset(b'First);
    Last      :          Stream_Element_Offset:= Stream_Element_Offset(b'Last);
    SE_Buffer : Stream_Element_Array (First..Last);
  begin
    Read(Stream(f_in).all, SE_Buffer, Last);
    for i in First..Last loop
      b(Natural(i)):= Unsigned_8(SE_Buffer(i));
    end loop;
    last_read:= Natural(last);
  end Read_SE_Copy;

  procedure Write_SE_Copy( b: in Buffer ) is
    use Ada.Streams;
    First     : constant Stream_Element_Offset:= Stream_Element_Offset(b'First);
    Last      : constant Stream_Element_Offset:= Stream_Element_Offset(b'Last);
    SE_Buffer : Stream_Element_Array (First..Last);
  begin
    if SE_buffer_size = 0 then
      SE_buffer_size:= SE_Buffer'size; -- just for stats
    end if;
    for i in SE_Buffer'Range loop
      SE_Buffer(i):= Stream_Element(b(Natural(i)));
    end loop;
    Write(Stream(f_out).all, SE_Buffer);
  end Write_SE_Copy;

  -- Overlay idea by Jeff Carter

  procedure Read_SE_Overlay( b: out Buffer; last_read: out Natural ) is
    use Ada.Streams;
    Last: Stream_Element_Offset;
    SE_Buffer : Stream_Element_Array (1..b'Length);
    for SE_Buffer'Address use b'Address;
  begin
    Read(Stream(f_in).all, SE_Buffer, Last);
    last_read:= b'First + Natural(Last) - 1;
  end Read_SE_Overlay;

  procedure Write_SE_Overlay( b: in Buffer ) is
    use Ada.Streams;
    SE_Buffer : Stream_Element_Array (1..b'Length);
    for SE_Buffer'Address use b'Address;
  begin
    Write(Stream(f_out).all, SE_Buffer);
  end Write_SE_Overlay;

  -- Using Unchecked_Conversion

  procedure Write_SE_UC( b: in Buffer ) is
    subtype My_SEA is Ada.Streams.Stream_Element_Array(1..b'Length);
    function To_SEA is new Ada.Unchecked_Conversion(Buffer, My_SEA);
    use Ada.Streams;
  begin
    Write(Stream(f_out).all, To_SEA(b));
  end Write_SE_UC;

  ----------
  -- Test --
  ----------

  function name return String is
  begin
    return Argument(1);
  end;

  generic
    label: String;
    with procedure Read( b: out Buffer; last_read: out Natural  );
    with procedure Write( b: in Buffer  );
  procedure Test;

  procedure Test is
    b: Buffer(1..1024);
    l: Natural;
  begin
    Open(f_in, In_File, name);
    Create(f_out, Out_File, name & "_$$$_" & label & ".tmp");
    while not End_of_File(f_in) loop
      Read(b,l);
      Write(b(1..l));
    end loop;
    Close(f_out);
    Close(f_in);
  end;

  procedure Test_Attribute is new Test("Attribute", Read_Attribute, Write_Attribute);
  procedure Test_SE_Copy is new Test("SE_Copy", Read_SE_Copy, Write_SE_Copy);
  procedure Test_SE_Overlay is new Test("SE_Overlay", Read_SE_Overlay, Write_SE_Overlay);
  procedure Test_SE_UC is new Test("SE_UC", Read_SE_Overlay, Write_SE_UC);

  T0, T1, T2, T3, T4: Time;

  use Ada.Text_IO;

begin
  if Argument_Count=0 then
    Put_Line(" Usage: test_stream_performance <big_file>");
    Put_Line(" Produces .tmp files that are copies of <big_file>.");
    return;
  end if;
  T0:= Clock;
  Test_Attribute;
  T1:= Clock;
  Test_SE_Copy;
  T2:= Clock;
  Test_SE_Overlay;
  T3:= Clock;
  Test_SE_UC;
  T4:= Clock;
  Put_Line("xxx'Write / xxx'Read (Stream attributes)........." & Duration'Image(T1-T0) & " seconds");
  Put_Line("Workarounds with Stream_Element_Array buffer:");
  Put_Line("  copy..........................................." & Duration'Image(T2-T1) & " seconds");
  Put_Line("  overlay (read), unchecked_conversion (write)..." & Duration'Image(T4-T3) & " seconds");
  Put_Line("  overlay........................................" & Duration'Image(T3-T2) & " seconds");
  Put_Line("Factor (Copy)   " & Duration'Image((T1-T0)/(T2-T1)));
  Put_Line("Factor (Overlay)" & Duration'Image((T1-T0)/(T3-T2)));
  New_Line;
  Put_Line("Buffer size in bits....." & Integer'Image(buffer_size));
  Put_Line("SE Buffer size in bits.." & Integer'Image(SE_buffer_size));
  New_Line;
  Open(f_in, In_File, name);
  Put_Line("File size in megabytes....." & Float'Image(Float(Size(f_in))/(1024.0*1024.0)));
  Close(f_in);
end;
