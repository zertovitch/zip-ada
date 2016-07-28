--  DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS

with LZ77;

package body LZMA_Encoding is

  procedure Encode is

    procedure LZ77_emits_literal_byte (b: Byte) is
    begin
      null; -- !!
    end LZ77_emits_literal_byte;

    procedure LZ77_emits_DL_code (distance, length: Integer) is
    begin
      null; -- !!
    end LZ77_emits_DL_code;

    ------------------------
    --  LZ77 compression  --
    ------------------------

    LZ77_choice: constant array(LZMA_Level) of LZ77.Method_Type:=
      (Level_1   => LZ77.IZ_6,
       Level_2   => LZ77.IZ_10);

    Look_Ahead         : constant Integer:= 258;
    String_buffer_size : constant := 2**15;  --  2**15 = size for Deflate

    procedure My_LZ77 is
      new LZ77.Encode
        ( String_buffer_size => String_buffer_size,
          Look_Ahead         => Look_Ahead,
          Threshold          => 2,  --  From a string match length > 2, a DL code is sent
          Method             => LZ77_choice(level),
          Read_byte          => Read_byte,
          More_bytes         => More_bytes,
          Write_byte         => LZ77_emits_literal_byte,
          Write_code         => LZ77_emits_DL_code
        );
  begin
    My_LZ77;
  end Encode;

end LZMA_Encoding;
