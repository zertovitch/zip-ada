--  Universal Trained Compression   ( -- DRAFT -- )
---------------------------------
--  This package provides a generic streaming encoder-decoder
--  with the capability of training the decoder with data known
--  in advance, in order to achieve better compression of the
--  actual data to be transmitted if the known data is similar
--  to the training data.
--
with Interfaces;

package Trained_Compression is

  subtype Byte is Interfaces.Unsigned_8;

  ------------------------------
  --  Encoding - compression  --
  ------------------------------

  generic
    type Data_Bytes_Count is range <>;
    --  Input of training or data bytes:
    with function Read_Uncompressed_Training return Byte;
    with function Read_Uncompressed_Data return Byte;
    with function More_Uncompressed_Data_Bytes return Boolean;
    --  Output of compressed data:
    with procedure Write_Compressed_Byte (B : Byte);
    --  Untrained encoder:
    with procedure Encode_Untrained;
    --
  procedure Encode (Train_Uncompressed, Skip_Transmitted : Data_Bytes_Count);

  --------------------------------
  --  Decoding - decompression  --
  --------------------------------

  generic
    type Data_Bytes_Count is range <>;
    --  Input of training or data bytes:
    with function Read_Compressed_Training return Byte;
    with function Read_Compressed_Data return Byte;
    with function More_Compressed_Data_Bytes return Boolean;
    --  Output of compressed data:
    with procedure Write_Decompressed_Byte (B : Byte);
    --  Untrained decoder:
    with procedure Decode_Untrained;
    --
  procedure Decode (Train_Compressed, Skip_Decompressed : Data_Bytes_Count);

end Trained_Compression;