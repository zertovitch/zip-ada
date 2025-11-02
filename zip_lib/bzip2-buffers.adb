package body BZip2.Buffers is

  procedure Attach_New_Byte_Buffer (bit_buffer : in out Bit_Buffer_Type; size : Natural_32) is
  begin
    bit_buffer.destination_data  := new Buffer_Array (1 .. size);
    bit_buffer.destination_index := 0;
  end Attach_New_Byte_Buffer;

  procedure Flush_Bit_Buffer (bit_buffer : in out Bit_Buffer_Type) is
    use Interfaces;
  begin
    bit_buffer.destination_index := bit_buffer.destination_index + 1;
    bit_buffer.destination_data (bit_buffer.destination_index) := bit_buffer.buffer;
    bit_buffer.buffer  := 0;
    bit_buffer.bit_index := 7;
  end Flush_Bit_Buffer;

  procedure Put_Bits (bit_buffer : in out Bit_Buffer_Type; data : Interfaces.Unsigned_32; amount : Positive) is
    use Interfaces;
  begin
    for count in reverse 1 .. amount loop
      if (data and Shift_Left (Unsigned_32'(1), count - 1)) /= 0 then
        bit_buffer.buffer := bit_buffer.buffer or Shift_Left (Unsigned_8'(1), bit_buffer.bit_index);
      end if;
      if bit_buffer.bit_index = 0 then
        Flush_Bit_Buffer (bit_buffer);
      else
        bit_buffer.bit_index := bit_buffer.bit_index - 1;
      end if;
    end loop;
  end Put_Bits;

  procedure Put_Bits (bit_buffer : in out Bit_Buffer_Type; b : Boolean) is
  begin
    Put_Bits (bit_buffer, Boolean'Pos (b), 1);
  end Put_Bits;

  procedure Put_Bits (bit_buffer : in out Bit_Buffer_Type; s : String) is
  begin
    for c of s loop
      Put_Bits (bit_buffer, Character'Pos (c), 8);
    end loop;
  end Put_Bits;

end BZip2.Buffers;
