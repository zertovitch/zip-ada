-- Usage: several_sizes <big_file>
-- Produces .tmp files that are partial copies of <big_file>, with different sizes.
-- These files can be compressed for testing.

with Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Interfaces;                        use Interfaces;

procedure Several_sizes is

  f_in, f_out: Ada.Streams.Stream_IO.File_Type;

  type Buffer is array(Natural range <>) of Unsigned_8;

  procedure Read( b: out Buffer; last_read: out Natural ) is
    use Ada.Streams;
    Last: Stream_Element_Offset;
    SE_Buffer : Stream_Element_Array (1..b'Length);
    for SE_Buffer'Address use b'Address;
    pragma Import (Ada, SE_Buffer);
  begin
    Read(Stream(f_in).all, SE_Buffer, Last);
    last_read:= b'First + Natural(Last) - 1;
  end Read;

  procedure Write( b: in Buffer ) is
    use Ada.Streams;
    SE_Buffer : Stream_Element_Array (1..b'Length);
    for SE_Buffer'Address use b'Address;
    pragma Import (Ada, SE_Buffer);
  begin
    Write(Stream(f_out).all, SE_Buffer);
  end Write;

  ----------
  -- Test --
  ----------

  function name return String is
  begin
    return Argument(1);
  end;

  procedure Test(limit: Natural) is
    b: Buffer(1..1024);
    l, rest: Natural;
    s: String(1..15);
  begin
    Ada.Integer_Text_IO.Put(s,limit+1_000_000_000);
    rest:= limit;
    Open(f_in, In_File, name);
    Create(f_out, Out_File, s(7..15) & ".tmp");
    while not End_Of_File(f_in) loop
      Read(b,l);
      if rest < l then
        Write(b(1..rest));
        exit;
      end if;
      Write(b(1..l));
      rest:= rest - l;
    end loop;
    Close(f_out);
    Close(f_in);
  end;

  use Ada.Text_IO;

  s: Positive;

begin
  if Argument_Count=0 then
    Put_Line(" Usage: several_sizes <big_file>");
    Put_Line(" Produces .tmp files that are partial copies of <big_file>, with different sizes.");
    return;
  end if;
  --
  for i in 0..126 loop
    Test(i);
  end loop;
  --
  s:= 128;
  loop
    for i in -60..60 loop
      Test(s+i);
    end loop;
    s:= s * 2;
    exit when s > 300_000;
  end loop;
end;
