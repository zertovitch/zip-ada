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
    --
    --  Important: Skip_Compressed needs to be slightly less
    --  than the full compressed trainer's actual size.
    --  Reason: the trainer alone has an end-of-stream symbol;
    --  furthermore, the trainer, with data after it, may have a
    --  longer LZ77 match that the trainer alone would not have
    --  near its end. You can append a bit of noise to
    --  the uncompressed trainer data to avoid having to reduce
    --  Skip_Compressed too much.
    --
  procedure Encode (Train_Uncompressed, Skip_Compressed : Data_Bytes_Count);

  --------------------------------
  --  Decoding - decompression  --
  --------------------------------

  generic
    type Data_Bytes_Count is range <>;
    --  Input of training or data bytes:
    with function Read_Compressed_Training return Byte;
    with function Read_Compressed_Data return Byte;
    --  Output of compressed data:
    with procedure Write_Decompressed_Byte (B : Byte);
    --
    --  Important: Train_Compressed needs to be equal to
    --             the encoder's Skip_Compressed value.
    --
  procedure Decode (Train_Compressed, Skip_Decompressed : Data_Bytes_Count);

end Trained_Compression;
