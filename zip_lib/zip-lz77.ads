--  Generic LZ77 encoder.

generic

  ----- LZSS Parameters -----
  String_buffer_size : Integer := 2**12;
  Look_Ahead         : Integer := 65;
  Threshold          : Integer := 2;

  Method: LZ77_method;

  -- Input of data:
  with function  Read_byte return Byte;
  with function  More_bytes return Boolean;
  -- Output of LZ-compressed data:
  with procedure Write_byte( b: Byte );
  with procedure Write_code( distance, length: Integer );

procedure Zip.LZ77;
