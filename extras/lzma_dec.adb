-- Standalone, command-line, LZMA Decoder (for .lzma files).

with LZMA_Dec_Pkg;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Interfaces;                        use Interfaces;

procedure LZMA_Dec is

  use LZMA_Dec_Pkg, LZMA_Dec_Pkg.My_LZMA_Decoding;

  procedure Print_Data_Bytes_Count(title: String; v: Data_Bytes_Count) is
    package CIO is new Integer_IO(Data_Bytes_Count);
  begin
    Put(title);
    Put(" : ");
    CIO.Put(v, 0);
    Put(" bytes");
    New_Line;
  end Print_Data_Bytes_Count;

  lzmaDecoder: CLzmaDecoder;
  res: LZMA_Result;

  use type BIO.Count; 

begin
  New_Line;
  Put_Line("LZMA_Dec, from the LZMA Reference Decoder by Igor Pavlov.");
  if Argument_Count = 0 then
    Put_Line("Use: lzma_dec a.lzma outfile");
    return;
  elsif Argument_Count /= 2 then
    Put_Line("You must specify two parameters");
    return;
  end if;
  Open(f_in, In_File, Argument(1));
  Create(f_out,Out_File, Argument(2));
  
  Decode_Header(lzmaDecoder);

  Put_Line(
    "lc="   & Natural'Image(Literal_context_bits(lzmaDecoder)) & 
    ", lp=" & Natural'Image(Literal_pos_bits(lzmaDecoder)) &
    ", pb=" & Natural'Image(Pos_bits(lzmaDecoder)) 
  );
  Put_Line("Dictionary size in properties =" & Unsigned_32'Image(Dictionary_size_in_properties(lzmaDecoder)));
  Put_Line("Dictionary size for decoding  =" & Unsigned_32'Image(Dictionary_size(lzmaDecoder)));
  New_Line;

  if Unpack_size_defined(lzmaDecoder) then
    Print_Data_Bytes_Count("Uncompressed size", Unpack_size_as_defined(lzmaDecoder));
  else
    Put_Line("Uncompressed size not defined, end marker is expected.");
  end if;
  New_Line;

  Create_Large_Arrays(lzmaDecoder);
  Decode_Contents(lzmaDecoder, res);
  
  Print_Data_Bytes_Count("Read    ", Data_Bytes_Count(Index(f_in) - 1));
  Print_Data_Bytes_Count("Written ", Data_Bytes_Count(Index(f_out) - 1));
  case res is
    when LZMA_finished_without_marker =>
       Put_Line("Finished without end marker");
    when LZMA_finished_with_marker =>
       if Unpack_size_defined(lzmaDecoder) then
         if Data_Bytes_Count(Index(f_out) - 1) /= Unpack_size_as_defined(lzmaDecoder) then
           Put_Line("Warning: finished with end marker before than specified size");
         end if;
       end if;
       Put_Line("Finished with end marker");
  end case;

  if Range_decoder_corrupted(lzmaDecoder) then
    Put_Line("Warning: LZMA stream is corrupted");
  end if;
  
  Close(f_in);
  Close(f_out);
end LZMA_Dec;
