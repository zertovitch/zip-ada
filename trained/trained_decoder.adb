--  Trained_Decoder
-------------------
--  This is a demo showing Trained_Compression used on the decoding side.

with Trained_Compression;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Interfaces;

procedure Trained_Decoder is

  --  NB: The Byte I/O below is not buffered, so it is very slow.
  --  You need to implement a circular buffer of type Stream_Element_Array for a fast I/O.
  --  For instance, see the BlockRead in the Zip package for how to do it.

  subtype Byte is Interfaces.Unsigned_8;
  package Byte_IO is new Ada.Direct_IO (Byte);

  Infile_Train, Infile_Data, Outfile: Byte_IO.File_Type;

  function Read_Train_Byte return Byte is
    B: Byte;
  begin
    Byte_IO.Read (Infile_Train, B);
    return B;
  end Read_Train_Byte;

  function Read_Data_Byte return Byte is
    B: Byte;
  begin
    Byte_IO.Read (Infile_Data, B);
    return B;
  end Read_Data_Byte;

  procedure Write_Byte (B: Byte) is
  begin
    Byte_IO.Write (Outfile, B);
  end Write_Byte;

  procedure TCD is new Trained_Compression.Decode (
    Data_Bytes_Count             => Byte_IO.Count,
    Read_Compressed_Training     => Read_Train_Byte,
    Read_Compressed_Data         => Read_Data_Byte,
    Write_Decompressed_Byte      => Write_Byte);

begin
  if Argument_Count < 5 then
    Put_Line ("Syntax:");
    Put_Line ("trained_decoder train_file data_file decompressed_file");
    Put_Line ("                train_compressed_size skip_decompressed_size");
    New_Line;
    Put_Line ("Important: train_compressed_size needs to be equal to");
    Put_Line ("           the encoder's skip_compressed_size");
  else
    Byte_IO.Open (Infile_Train,  Byte_IO.In_File,  Name => Argument (1));
    Byte_IO.Open (Infile_Data,   Byte_IO.In_File,  Name => Argument (2));
    Byte_IO.Create (Outfile,     Byte_IO.Out_File, Name => Argument (3));
    TCD (
      --  We need to use less than the full compressed training data.
      Train_Compressed  => Byte_IO.Count'Value (Argument (4)),
      Skip_Decompressed => Byte_IO.Count'Value (Argument (5))
    );
    Byte_IO.Close (Infile_Train);
    Byte_IO.Close (Infile_Data);
    Byte_IO.Close (Outfile);
  end if;
end Trained_Decoder;
