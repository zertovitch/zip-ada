-- LZMA_Decoding - Ada 95 translation of LzmaSpec.cpp, LZMA Reference Decoder 9.31
-- LzmaSpec.cpp : 2013-07-28 : Igor Pavlov : Public domain

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;                    use Ada.Exceptions;

package body LZMA_Decoding is

  procedure Create(o: in out COutWindow; dictSize: UInt32) is
  begin
    o.Buf      := new Byte_buffer(0..dictSize-1);
    o.Pos      := 0;
    o.Size     := dictSize;
    o.IsFull   := False;
    o.TotalPos := 0;
  end Create;

  procedure PutByte(o: in out COutWindow; b: Byte) is
  pragma Inline(PutByte);
  begin
    o.TotalPos := o.TotalPos + 1;
    o.Buf(o.Pos):= b;
    o.Pos := o.Pos + 1;
    if o.Pos = o.Size then
      o.Pos := 0;
      o.IsFull := True;
    end if;
    Write_Byte(b);
  end PutByte;

  function GetByte(o: COutWindow; dist: UInt32) return Byte is
  pragma Inline(GetByte);
  begin
    if dist <= o.Pos then
      return o.Buf(o.Pos - dist);
    else
      return o.Buf(o.Size - dist + o.Pos);
    end if;
  end GetByte;

  procedure CopyMatch(o: in out COutWindow; dist: UInt32; len: Unsigned) is
  pragma Inline(CopyMatch);
  begin
    for count in reverse 1..len loop
      PutByte(o, GetByte(o, dist));
    end loop;
  end CopyMatch;

  function CheckDistance(o: COutWindow; dist: UInt32) return Boolean is
  pragma Inline(CheckDistance);
  begin
    return dist <= o.Pos or o.IsFull;
  end;

  function IsEmpty(o: COutWindow) return Boolean is
  pragma Inline(IsEmpty);
  begin
    return o.Pos = 0 and then not o.IsFull;
  end;

  kNumBitModelTotalBits : constant:= 11;
  kNumMoveBits          : constant:= 5;

  PROB_INIT_VAL : constant := (2 ** kNumBitModelTotalBits) / 2;

  procedure Init(o: in out CRangeDecoder) is
  begin
    o.Corrupted := False;
    if Read_Byte /= 0 then
      o.Corrupted := True;
    end if;
    o.RangeZ := 16#FFFF_FFFF#;
    o.Code   := 0;
    for i in 0..3 loop
      o.Code := Shift_Left(o.Code, 8) or UInt32(Read_Byte);
    end loop;
    if o.Code = o.RangeZ then
      o.Corrupted := True;
    end if;
  end Init;

  function IsFinishedOK(o: CRangeDecoder) return Boolean is
  pragma Inline(IsFinishedOK);
  begin
    return o.Code = 0;
  end;

  kTopValue : constant := 2**24;

  procedure Normalize(o: in out CRangeDecoder) is
  pragma Inline(Normalize);
  begin
    if o.RangeZ < kTopValue then
      o.RangeZ := Shift_Left(o.RangeZ, 8);
      o.Code  := Shift_Left(o.Code, 8) or UInt32(Read_Byte);
    end if;
  end Normalize;

  procedure DecodeDirectBits(o: in out CRangeDecoder; numBits : Natural; res: out UInt32) is
  pragma Inline(DecodeDirectBits);
    t: UInt32;
  begin
    res := 0;
    for count in reverse 1..numBits loop
      o.RangeZ := Shift_Right(o.RangeZ, 1);
      o.Code := o.Code - o.RangeZ;
      t := - Shift_Right(o.Code, 31);
      o.Code := o.Code + (o.RangeZ and t);
      if o.Code = o.RangeZ then
        o.Corrupted := True;
      end if;
      Normalize(o);
      res := res + res + t + 1;
    end loop;
  end DecodeDirectBits;

  kNumBitModel_Count: constant:= 2 ** kNumBitModelTotalBits;

  procedure DecodeBit(o: in out CRangeDecoder; prob: in out CProb; symbol: out Unsigned) is
  pragma Inline(DecodeBit);
    v: UInt32 := UInt32(prob); -- unsigned in the C++ code
    bound: constant UInt32:= Shift_Right(o.RangeZ, kNumBitModelTotalBits) * v;
  begin
    if o.Code < bound then
      v:= v + Shift_Right(kNumBitModel_Count - v, kNumMoveBits);
      o.RangeZ := bound;
      prob := CProb(v);
      Normalize(o);
      symbol := 0;
    else
      v:= v - Shift_Right(v, kNumMoveBits);
      o.Code := o.Code - bound;
      o.RangeZ := o.RangeZ - bound;
      prob := CProb(v);
      Normalize(o);
      symbol := 1;
    end if;
  end DecodeBit;

  procedure BitTreeReverseDecode(prob: in out CProb_array; numBits : Natural; rc: in out CRangeDecoder; symbol: in out UInt32) is
  pragma Inline(BitTreeReverseDecode);
    m: Unsigned := 1;
    bit: Unsigned;
  begin
    for i in 0..numBits-1 loop
      DecodeBit(rc, prob(m+prob'First), bit);
      m := m + m + bit;
      symbol := symbol or Shift_Left(UInt32(bit), i);
    end loop;
  end BitTreeReverseDecode;

  procedure BitTreeDecode(prob: in out CProb_array; NumBits: Positive; rc: in out CRangeDecoder; m: out Unsigned) is
  pragma Inline(BitTreeDecode);
    bit: Unsigned;
  begin
    m:= 1;
    for count in reverse 1..NumBits loop
      DecodeBit(rc, prob(m), bit);
      m:= m + m + bit;
    end loop;
    m:= m - 2**NumBits;
  end BitTreeDecode;

  kMatchMinLen        : constant := 2;

  procedure Init(o: in out CLenDecoder) is
  begin
    o.Choice    := PROB_INIT_VAL;
    o.Choice2   := PROB_INIT_VAL;
    o.HighCoder := (others => PROB_INIT_VAL);
    o.LowCoder  := (others => (others => PROB_INIT_VAL));
    o.MidCoder  := (others => (others => PROB_INIT_VAL));
  end Init;

  procedure Decode(o: in out CLenDecoder; rc: in out CRangeDecoder; posState: Unsigned; res: out Unsigned) is
  pragma Inline(Decode);
    bit_a, bit_b: Unsigned;
  begin
    DecodeBit(rc, o.Choice, bit_a);
    if bit_a = 0 then
      BitTreeDecode(o.LowCoder(posState), 3, rc, res);
      return;
    end if;
    DecodeBit(rc, o.Choice2, bit_b);
    if bit_b = 0 then
      BitTreeDecode(o.MidCoder(posState), 3, rc, res);
      res:= res + 8;
      return;
    end if;
    BitTreeDecode(o.HighCoder, 8, rc, res);
    res:= res + 16;
  end Decode;

  subtype State_range is Unsigned range 0..kNumStates-1;
  type Transition is array(State_range) of State_range;

  UpdateState_Literal  : constant Transition:= (0, 0, 0, 0, 1, 2, 3, 4,  5,  6,   4, 5);
  UpdateState_Match    : constant Transition:= (7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10);
  UpdateState_Rep      : constant Transition:= (8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11);
  UpdateState_ShortRep : constant Transition:= (9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11);

  LZMA_DIC_MIN : constant := 2 ** 12;

  procedure Finalize_Manually(o: in out LZMA_Decoder_Info) is
    procedure Dispose is new Ada.Unchecked_Deallocation(CProb_array, p_CProb_array);
    procedure Dispose is new Ada.Unchecked_Deallocation(Byte_buffer, p_Byte_buffer);
  begin
    Dispose(o.LitProbs);
    Dispose(o.OutWindow.Buf);
  end;

  procedure Decode_Properties(o: in out LZMA_Decoder_Info; b: Byte_buffer) is
    d: Unsigned := Unsigned(b(b'First));
  begin
    if d >= 9 * 5 * 5 then
      Raise_Exception(LZMA_Error'Identity, "Incorrect LZMA properties");
      -- raise LZMA_Error with "Incorrect LZMA properties"; -- Ada 2005+
    end if;
    o.lc := LC_range(d mod 9);
    d := d / 9;
    o.lp := LP_range(d mod 5);
    o.literal_pos_mask:= 2 ** o.lp - 1;
    o.pb := PB_range(d / 5);
    o.pos_bits_mask:= 2 ** o.pb - 1;
    o.dictSizeInProperties := 0;
    for i in 0..3 loop
      o.dictSizeInProperties := o.dictSizeInProperties +
        UInt32(b(UInt32(i) + 1 + b'First)) * 2 ** (8 * i);
    end loop;
    o.dictSize := o.dictSizeInProperties;
    if o.dictSize < LZMA_DIC_MIN then
      o.dictSize := LZMA_DIC_MIN;
    end if;
  end Decode_Properties;

  procedure Create_Large_Arrays(o: in out LZMA_Decoder_Info) is
    length: constant Unsigned:= 16#300# * 2 ** (o.lc + o.lp);
  begin
    Create(o.OutWindow, o.dictSize);
    o.LitProbs := new CProb_array(0..length-1); -- Literals
  end Create_Large_Arrays;

  procedure DecodeLiteral(o: in out LZMA_Decoder_Info; state: Unsigned; rep0: UInt32) is
  pragma Inline(DecodeLiteral);
    prevByte     : Byte:= 0;
    symbol       : Unsigned:= 1;
    litState     : Unsigned;
    probs_idx    : Unsigned;
    matchByte    : UInt32;
    matchBit     : UInt32;
    bit_a, bit_b : Unsigned;
  begin
    if not IsEmpty(o.OutWindow) then
      prevByte := GetByte(o.OutWindow, 1);
    end if;
    litState :=
      Unsigned(
        Shift_Left(UInt32(o.OutWindow.TotalPos) and o.literal_pos_mask, o.lc) +
        Shift_Right(UInt32(prevByte), 8 - o.lc)
      );
    probs_idx:= 16#300# * litState;
    if state >= 7 then
      matchByte := UInt32(GetByte(o.OutWindow, rep0 + 1));
      loop
        matchBit  := Shift_Right(matchByte, 7) and 1;
        matchByte := matchByte + matchByte;
        DecodeBit(o.RangeDec,
          o.LitProbs(probs_idx + Unsigned(Shift_Left(1 + matchBit, 8)) + symbol),
          bit_a
        );
        symbol := (symbol + symbol) or bit_a;
        exit when (Unsigned(matchBit) /= bit_a) or else (symbol >= 16#100#);
      end loop;
    end if;
    while symbol < 16#100# loop
      DecodeBit(o.RangeDec, o.LitProbs(probs_idx + symbol), bit_b);
      symbol := (symbol + symbol) or bit_b;
    end loop;
    PutByte(o.OutWindow, Byte(symbol - 16#100#));
  end DecodeLiteral;

  procedure DecodeDistance(o: in out LZMA_Decoder_Info; len: Unsigned; dist: out UInt32) is
  pragma Inline(DecodeDistance);
    lenState      : Unsigned := len;
    posSlot       : Unsigned;
    numDirectBits : Natural;
    deco          : UInt32;
  begin
    if lenState > kNumLenToPosStates - 1 then
      lenState := kNumLenToPosStates - 1;
    end if;
    BitTreeDecode(o.PosSlotDecoder(lenState), 6, o.RangeDec, posSlot);
    if posSlot < 4 then
      dist:= UInt32(posSlot);
      return;
    end if;
    numDirectBits := Natural(Shift_Right(UInt32(posSlot), 1) - 1);
    dist := Shift_Left(2 or (UInt32(posSlot) and 1), numDirectBits);
    if posSlot < kEndPosModelIndex then
      BitTreeReverseDecode(
        o.PosDecoders(Unsigned(dist) - posSlot .. Last_PosDecoders),
        numDirectBits, o.RangeDec, dist
      );
    else
      DecodeDirectBits(o.RangeDec, numDirectBits - kNumAlignBits, deco);
      dist:= dist + Shift_Left(deco, kNumAlignBits);
      BitTreeReverseDecode(o.AlignDecoder, kNumAlignBits, o.RangeDec, dist);
    end if;
  end DecodeDistance;

  procedure Init(o: in out LZMA_Decoder_Info) is
  begin
    -- Literals:
    o.LitProbs.all := (others => PROB_INIT_VAL);
    -- Distances:
    o.PosSlotDecoder := (others => (others => PROB_INIT_VAL));
    o.AlignDecoder   := (others => PROB_INIT_VAL);
    o.PosDecoders    := (others => PROB_INIT_VAL);
    --
    o.IsMatch    := (others => PROB_INIT_VAL);
    o.IsRep      := (others => PROB_INIT_VAL);
    o.IsRepG0    := (others => PROB_INIT_VAL);
    o.IsRepG1    := (others => PROB_INIT_VAL);
    o.IsRepG2    := (others => PROB_INIT_VAL);
    o.IsRep0Long := (others => PROB_INIT_VAL);
    Init(o.LenDecoder);
    Init(o.RepLenDecoder);
  end Init;

  procedure Decode_Contents(o: in out LZMA_Decoder_Info; res: out LZMA_Result) is
    rep0, rep1, rep2, rep3 : UInt32 := 0;
    state : State_Range := 0;
    posState: State_Range;
    use type BIO.Count;
    Marker_exit: exception;

    procedure Process_Litteral is
    pragma Inline(Process_Litteral);
    begin
      if o.unpackSize = 0 and then o.unpackSizeDefined then
        Raise_Exception(
          LZMA_Error'Identity,
          "Decoded data will exceed expected data size (Process_Litteral)"
        );
      end if;
      DecodeLiteral(o, state, rep0);
      state := UpdateState_Literal(state);
      o.unpackSize:= o.unpackSize - 1;
    end Process_Litteral;

    procedure Process_Distance_and_Length is
    pragma Inline(Process_Distance_and_Length);
      len: Unsigned;
      isError: Boolean;
      dist: UInt32;
      bit_a, bit_b, bit_c, bit_d, bit_e: Unsigned;
    begin
      DecodeBit(o.RangeDec, o.IsRep(state), bit_a);
      if bit_a /= 0 then
        if o.unpackSize = 0 and then o.unpackSizeDefined then
          Raise_Exception(
            LZMA_Error'Identity,
            "Decoded data will exceed expected data size (in Process_Distance_and_Length, #1)"
          );
        end if;
        if IsEmpty(o.OutWindow) then
          Raise_Exception(
            LZMA_Error'Identity,
            "Output window buffer is empty (in Process_Distance_and_Length)"
          );
        end if;
        DecodeBit(o.RangeDec, o.IsRepG0(state), bit_b);
        if bit_b = 0 then
          DecodeBit(o.RangeDec, o.IsRep0Long(state * kNumPosBitsMax_Count + posState), bit_c);
          if bit_c = 0 then
            state := UpdateState_ShortRep(state);
            PutByte(o.OutWindow, GetByte(o.OutWindow, rep0 + 1));
            o.unpackSize:= o.unpackSize - 1;
            return;  -- GdM: this way, we go to the next iteration (C++: continue)
          end if;
        else
          DecodeBit(o.RangeDec, o.IsRepG1(state), bit_d);
          if bit_d = 0 then
            dist := rep1;
          else
            DecodeBit(o.RangeDec, o.IsRepG2(state), bit_e);
            if bit_e = 0 then
              dist := rep2;
            else
              dist := rep3;
              rep3 := rep2;
            end if;
            rep2 := rep1;
          end if;
          rep1 := rep0;
          rep0 := dist;
        end if;
        Decode(o.RepLenDecoder, o.RangeDec, posState, len);
        state := UpdateState_Rep(state);
      else
        rep3 := rep2;
        rep2 := rep1;
        rep1 := rep0;
        Decode(o.LenDecoder, o.RangeDec, posState, len);
        state := UpdateState_Match(state);
        DecodeDistance(o, len, rep0);
        if rep0 = 16#FFFF_FFFF# then
          if IsFinishedOK(o.RangeDec) then
            raise Marker_exit;
          else
            Raise_Exception(
              LZMA_Error'Identity,
              "Range decoder not finished on EOS marker (in Process_Distance_and_Length)"
            );
          end if;
        end if;
        if (o.unpackSize = 0 and then o.unpackSizeDefined) or
            rep0 >= o.dictSize or not CheckDistance(o.OutWindow, rep0)
        then
          Raise_Exception(
            LZMA_Error'Identity,
            "Decoded data will exceed expected data size (in Process_Distance_and_Length, #2)"
          );
        end if;
      end if;
      len := len + kMatchMinLen;
      isError := False;
      if o.unpackSize < Data_Bytes_Count(len) and then o.unpackSizeDefined then
        len := Unsigned(o.unpackSize);
        isError := True;
      end if;
      CopyMatch(o.OutWindow, rep0 + 1, len);
      o.unpackSize:= o.unpackSize - Data_Bytes_Count(len);
      if isError then
        Raise_Exception(
          LZMA_Error'Identity,
          "Decoded data will exceed expected data size (in Process_Distance_and_Length, #3)"
        );
      end if;
    end Process_Distance_and_Length;

    bit_choice: Unsigned;

  begin
    Init(o);
    Init(o.RangeDec);
    loop
      if o.unpackSize = 0 
        and then (o.unpackSizeDefined and not o.markerIsMandatory) 
        and then IsFinishedOK(o.RangeDec)
      then
        res:= LZMA_finished_without_marker;
        return;
      end if;
      posState := State_range(UInt32(o.OutWindow.TotalPos) and o.pos_bits_mask);
      DecodeBit(o.RangeDec, o.IsMatch(state * kNumPosBitsMax_Count + PosState), bit_choice);
      if bit_choice = 0 then
        Process_Litteral;
      else
        Process_Distance_and_Length;
      end if;
    end loop;
  exception
    when Marker_exit =>
      res:= LZMA_finished_with_marker;
  end Decode_Contents;

  procedure Decode_Header(o: in out LZMA_Decoder_Info; hints: LZMA_Hints) is
    header: Byte_buffer(0..12);
    b: Byte;
    use type BIO.Count;
    last_bit: Natural;
  begin
    o.unpackSize := 0;
    o.unpackSizeDefined := False;

    for i in header'Range loop
      header(i):= Read_Byte;
      exit when i = 4 and not hints.has_size;
    end loop;

    Decode_Properties(o, header);

    if hints.has_size then
      for i in UInt32'(0)..7 loop
        b:= header(5 + i);
        if b /= 16#FF# then
          o.unpackSizeDefined := True;
        end if;
      end loop;
      if o.unpackSizeDefined then
        for i in UInt32'(0)..7 loop
          b:= header(5 + i);
          if b /= 0 then
            for bit in 0..7 loop
              if (b and Shift_Left(Byte'(1),bit)) /= 0 then
                last_bit:= bit;
              end if;
            end loop;
            last_bit:= last_bit + Natural(8 * i);
            if last_bit > Data_Bytes_Count'Size - 1 then
              Raise_Exception(
                LZMA_Error'Identity,
                "Indicated size bits for decoded data," &
                Natural'Image(last_bit) &
                ", exceeds the maximum file size bits," &
                Natural'Image(Data_Bytes_Count'Size - 1)
              );
            else
              o.unpackSize := o.unpackSize + Data_Bytes_Count(b) * 2 ** Natural(8 * i);
            end if;
          end if;
        end loop;
        o.unpackSize_as_defined:= o.unpackSize;
      else
        o.unpackSize:= Data_Bytes_Count'Last;
      end if;
    else
      o.unpackSize:= hints.given_size;
      o.unpackSizeDefined:= True;
    end if;
    o.markerIsMandatory := hints.marker_expected or not o.unpackSizeDefined;
  end Decode_Header;

  procedure Decode(o: in out LZMA_Decoder_Info; hints: LZMA_Hints; res: out LZMA_Result) is
  begin
    Decode_Header(o, hints);
    Create_Large_Arrays(o);
    Decode_Contents(o, res);
    Finalize_Manually(o);
  end Decode;

  procedure Decompress(hints: LZMA_Hints) is
    o: LZMA_Decoder_Info;
    res: LZMA_Result;
  begin
    Decode(o, hints, res);
  end;

  function Literal_context_bits(o: LZMA_Decoder_Info) return Natural is
  begin
    return o.lc;
  end;

  function Literal_pos_bits(o: LZMA_Decoder_Info) return Natural is
  begin
    return o.lp;
  end;

  function Pos_bits(o: LZMA_Decoder_Info) return Natural is
  begin
    return o.pb;
  end;

  function Unpack_size_defined(o: LZMA_Decoder_Info) return Boolean is
  begin
    return o.unpackSizeDefined;
  end;

  function Unpack_size_as_defined(o: LZMA_Decoder_Info) return Data_Bytes_Count is
  begin
    return o.unpackSize_as_defined;
  end;

  function Dictionary_size(o: LZMA_Decoder_Info) return Interfaces.Unsigned_32 is
  begin
    return o.dictSize;
  end;

  function Dictionary_size_in_properties(o: LZMA_Decoder_Info) return Interfaces.Unsigned_32 is
  begin
    return o.dictSizeInProperties;
  end;

  function Range_decoder_corrupted(o: LZMA_Decoder_Info) return Boolean is
  begin
    return o.RangeDec.Corrupted;
  end;

end LZMA_Decoding;
