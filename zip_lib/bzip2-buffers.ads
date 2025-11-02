with Ada.Unchecked_Deallocation;
with Interfaces;

private package BZip2.Buffers is

  -------------------
  --  Byte buffer  --
  -------------------

  type Buffer_Array is array (Natural_32 range <>) of Byte;
  type Buffer_Access is access all Buffer_Array;

  procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Buffer_Array, Buffer_Access);

  ------------------
  --  Bit buffer  --
  ------------------

  subtype Bit_Index_Type is Natural range 0 .. 7;

  type Bit_Buffer_Type is record
    buffer            : Byte           := 0;
    bit_index         : Bit_Index_Type := 7;
    destination_data  : Buffer_Access  := null;
    destination_index : Natural_32     := 0;
  end record;

  procedure Attach_New_Byte_Buffer (bit_buffer : in out Bit_Buffer_Type; size : Natural_32);

  procedure Flush_Bit_Buffer (bit_buffer : in out Bit_Buffer_Type);

  procedure Put_Bits (bit_buffer : in out Bit_Buffer_Type; data : Interfaces.Unsigned_32; amount : Positive);

  procedure Put_Bits (bit_buffer : in out Bit_Buffer_Type; b : Boolean);

  procedure Put_Bits (bit_buffer : in out Bit_Buffer_Type; s : String);

end BZip2.Buffers;
