--  Generic LZ77 encoder.

--  To do: replace implementation with a faster algorithm.
--         The one of info-zip / zlib should be a very good candidate...

generic

  ----- LZSS Parameters -----
  String_buffer_size : Integer := 2**12;
  Look_Ahead         : Integer := 65;
  Threshold          : Integer := 2;

  -- Input:
  with function  Read_byte return Byte;
  with function  More_bytes return Boolean;
  -- Output:
  with procedure Write_byte( b: Byte );
  with procedure Write_code( distance, length: Integer );

procedure Zip.LZ77;
