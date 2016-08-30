--  Standalone LZ77 compression package.

with Interfaces;

package LZ77 is

  ----------------
  --  Encoding  --
  ----------------

  type Method_Type is (
    --  Use the LZHuf algorithm (see body for details and credits)
    LZHuf,
    --  Use the Info-Zip algorithm, levels 4-10 (see body for details and credits)
    IZ_4,
    IZ_5,
    IZ_6,
    IZ_7,
    IZ_8,
    IZ_9,
    IZ_10
  );

  subtype Byte is Interfaces.Unsigned_8;

  generic
    ----- LZSS Parameters -----
    String_buffer_size : Integer := 2**12;
    Look_Ahead         : Integer := 65;
    Threshold          : Integer := 2;
    --
    Method: Method_Type;
    --
    -- Input of data:
    with function  Read_byte return Byte;
    with function  More_bytes return Boolean;
    -- Output of LZ-compressed data:
    with procedure Write_literal( b: Byte );
    with procedure Write_DL_code( distance, length: Integer );
    --
  procedure Encode;

end LZ77;
