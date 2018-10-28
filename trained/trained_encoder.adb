--  !! STUB !!

--  Trained_Encoder
-------------------
--  This is a demo showing Trained_Compression used on the encoding side.

with Trained_Compression;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Interfaces;

procedure Trained_Encoder is

  --  NB: The Byte I/O below is not buffered, so it is very slow.
  --  You need to implement a circular buffer of type Stream_Element_Array for a fast I/O.
  --  For instance, see the BlockRead in the Zip package for how to do it.

  subtype Byte is Interfaces.Unsigned_8;
  package Byte_IO is new Ada.Sequential_IO (Byte);

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

  function More_Data_Bytes return Boolean is
  begin
    return not Byte_IO.End_Of_File (Infile_Data);
  end More_Data_Bytes;

  procedure Write_Byte(B: Byte) is
  begin
    Byte_IO.Write (Outfile, B);
  end Write_Byte;

begin
  null;
end;
