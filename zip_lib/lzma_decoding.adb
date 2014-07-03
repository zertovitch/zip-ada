-- LZMA_Decoding - Ada 95 translation of LzmaSpec.cpp, LZMA Reference Decoder 9.31
-- LzmaSpec.cpp : 2013-07-28 : Igor Pavlov : Public domain

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;                    use Ada.Exceptions;

package body LZMA_Decoding is

  procedure Create(o: in out Out_Window; dictSize: UInt32) is
  begin
    o.buf       := new Byte_buffer(0..dictSize-1);
    o.pos       := 0;
    o.size      := dictSize;
    o.is_full   := False;
    o.total_pos := 0;
  end Create;

  procedure Put_Byte(o: in out Out_Window; b: Byte) is
  pragma Inline(Put_Byte);
  begin
    o.total_pos := o.total_pos + 1;
    o.buf(o.pos):= b;
    o.pos := o.pos + 1;
    if o.pos = o.size then
      o.pos := 0;
      o.is_full := True;
    end if;
    Write_Byte(b);
  end Put_Byte;

  function Get_Byte(o: Out_Window; dist: UInt32) return Byte is
  pragma Inline(Get_Byte);
  begin
    if dist <= o.pos then
      return o.buf(o.pos - dist);
    else
      return o.buf(o.size - dist + o.pos);
    end if;
  end Get_Byte;

  procedure Copy_Match(o: in out Out_Window; dist: UInt32; len: Unsigned) is
  pragma Inline(Copy_Match);
  begin
    for count in reverse 1..len loop
      Put_Byte(o, Get_Byte(o, dist));
    end loop;
  end Copy_Match;

  function Check_Distance(o: Out_Window; dist: UInt32) return Boolean is
  pragma Inline(Check_Distance);
  begin
    return dist <= o.pos or o.is_full;
  end;

  function Is_Empty(o: Out_Window) return Boolean is
  pragma Inline(Is_Empty);
  begin
    return o.pos = 0 and then not o.is_full;
  end;

  kNumBitModelTotalBits : constant:= 11;
  kNumMoveBits          : constant:= 5;

  PROB_INIT_VAL : constant := (2 ** kNumBitModelTotalBits) / 2;

  procedure Init(o: in out Range_Decoder) is
  begin
    o.corrupted := False;
    if Read_Byte /= 0 then
      o.corrupted := True;
    end if;
    o.range_z := 16#FFFF_FFFF#;
    o.code    := 0;
    for i in 0..3 loop
      o.code := Shift_Left(o.code, 8) or UInt32(Read_Byte);
    end loop;
    if o.code = o.range_z then
      o.corrupted := True;
    end if;
  end Init;

  function Is_Finished_OK(o: Range_Decoder) return Boolean is
  pragma Inline(Is_Finished_OK);
  begin
    return o.code = 0;
  end;

  kTopValue : constant := 2**24;

  procedure Normalize(o: in out Range_Decoder) is
  pragma Inline(Normalize);
  begin
    if o.range_z < kTopValue then
      o.range_z := Shift_Left(o.range_z, 8);
      o.code  := Shift_Left(o.code, 8) or UInt32(Read_Byte);
    end if;
  end Normalize;

  procedure Decode_Direct_Bits(o: in out Range_Decoder; num_bits : Natural; res: out UInt32) is
  pragma Inline(Decode_Direct_Bits);
    t: UInt32;
  begin
    res := 0;
    for count in reverse 1..num_bits loop
      o.range_z := Shift_Right(o.range_z, 1);
      o.code := o.code - o.range_z;
      t := - Shift_Right(o.code, 31);
      o.code := o.code + (o.range_z and t);
      if o.code = o.range_z then
        o.corrupted := True;
      end if;
      Normalize(o);
      res := res + res + t + 1;
    end loop;
  end Decode_Direct_Bits;

  kNumBitModel_Count: constant:= 2 ** kNumBitModelTotalBits;

  procedure Decode_Bit(o: in out Range_Decoder; prob: in out CProb; symbol: out Unsigned) is
  pragma Inline(Decode_Bit);
    v: UInt32 := UInt32(prob); -- unsigned in the C++ code
    bound: constant UInt32:= Shift_Right(o.range_z, kNumBitModelTotalBits) * v;
  begin
    if o.code < bound then
      v:= v + Shift_Right(kNumBitModel_Count - v, kNumMoveBits);
      o.range_z := bound;
      prob := CProb(v);
      Normalize(o);
      symbol := 0;
    else
      v:= v - Shift_Right(v, kNumMoveBits);
      o.code := o.code - bound;
      o.range_z := o.range_z - bound;
      prob := CProb(v);
      Normalize(o);
      symbol := 1;
    end if;
  end Decode_Bit;

  procedure Bit_Tree_Reverse_Decode(
    prob     : in out CProb_array;
    num_bits : in     Natural;
    rc       : in out Range_Decoder;
    symbol   : in out UInt32)
  is
  pragma Inline(Bit_Tree_Reverse_Decode);
    m: Unsigned := 1;
    bit: Unsigned;
  begin
    for i in 0..num_bits-1 loop
      Decode_Bit(rc, prob(m + prob'First), bit);
      m := m + m + bit;
      symbol := symbol or Shift_Left(UInt32(bit), i);
    end loop;
  end Bit_Tree_Reverse_Decode;

  procedure Bit_Tree_Decode(
    prob     : in out CProb_array;
    num_bits :        Positive;
    rc       : in out Range_Decoder;
    m        :    out Unsigned)
  is
  pragma Inline(Bit_Tree_Decode);
    bit: Unsigned;
  begin
    m:= 1;
    for count in reverse 1..num_bits loop
      Decode_Bit(rc, prob(m + prob'First), bit);
      m:= m + m + bit;
    end loop;
    m:= m - 2**num_bits;
  end Bit_Tree_Decode;

  procedure Init(o: in out Length_Decoder) is
  begin
    o.choice     := PROB_INIT_VAL;
    o.choice_2   := PROB_INIT_VAL;
    o.high_coder := (others => PROB_INIT_VAL);
    o.low_coder  := (others => (others => PROB_INIT_VAL));
    o.mid_coder  := (others => (others => PROB_INIT_VAL));
  end Init;

  procedure Decode(
    o          : in out Length_Decoder;
    rc         : in out Range_Decoder;
    pos_state  :        Unsigned;
    res        :    out Unsigned)
  is
  pragma Inline(Decode);
    bit_a, bit_b: Unsigned;
  begin
    Decode_Bit(rc, o.choice, bit_a);
    if bit_a = 0 then
      Bit_Tree_Decode(o.low_coder(pos_state), 3, rc, res);
      return;
    end if;
    Decode_Bit(rc, o.choice_2, bit_b);
    if bit_b = 0 then
      Bit_Tree_Decode(o.mid_coder(pos_state), 3, rc, res);
      res:= res + 8;
      return;
    end if;
    Bit_Tree_Decode(o.high_coder, 8, rc, res);
    res:= res + 16;
  end Decode;

  subtype State_range is Unsigned range 0..kNumStates-1;
  type Transition is array(State_range) of State_range;

  Update_State_Literal  : constant Transition:= (0, 0, 0, 0, 1, 2, 3, 4,  5,  6,   4, 5);
  Update_State_Match    : constant Transition:= (7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10);
  Update_State_Rep      : constant Transition:= (8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11);
  Update_State_ShortRep : constant Transition:= (9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11);

  LZMA_DIC_MIN : constant := 2 ** 12;

  procedure Finalize_Manually(o: in out LZMA_Decoder_Info) is
    procedure Dispose is new Ada.Unchecked_Deallocation(CProb_array, p_CProb_array);
    procedure Dispose is new Ada.Unchecked_Deallocation(Byte_buffer, p_Byte_buffer);
  begin
    Dispose(o.LitProbs);
    Dispose(o.out_win.buf);
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
    Create(o.out_win, o.dictSize);
    o.LitProbs := new CProb_array(0..length-1); -- Literals
  end Create_Large_Arrays;

  procedure DecodeLiteral(o: in out LZMA_Decoder_Info; state: Unsigned; rep0: UInt32) is
  pragma Inline(DecodeLiteral);
    prevByte     : Byte:= 0;
    symbol       : Unsigned:= 1;
    lit_state    : Unsigned;
    probs_idx    : Unsigned;
    matchByte    : UInt32;
    matchBit     : UInt32;
    bit_a, bit_b : Unsigned;
  begin
    if not Is_Empty(o.out_win) then
      prevByte := Get_Byte(o.out_win, 1);
    end if;
    lit_state :=
      Unsigned(
        Shift_Left(UInt32(o.out_win.total_pos) and o.literal_pos_mask, o.lc) +
        Shift_Right(UInt32(prevByte), 8 - o.lc)
      );
    probs_idx:= 16#300# * lit_state;
    if state >= 7 then
      matchByte := UInt32(Get_Byte(o.out_win, rep0 + 1));
      loop
        matchBit  := Shift_Right(matchByte, 7) and 1;
        matchByte := matchByte + matchByte;
        Decode_Bit(o.range_dec,
          o.LitProbs(probs_idx + Unsigned(Shift_Left(1 + matchBit, 8)) + symbol),
          bit_a
        );
        symbol := (symbol + symbol) or bit_a;
        exit when (Unsigned(matchBit) /= bit_a) or else (symbol >= 16#100#);
      end loop;
    end if;
    while symbol < 16#100# loop
      Decode_Bit(o.range_dec, o.LitProbs(probs_idx + symbol), bit_b);
      symbol := (symbol + symbol) or bit_b;
    end loop;
    Put_Byte(o.out_win, Byte(symbol - 16#100#)); -- The output of a simple literal happens here.
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
    Bit_Tree_Decode(o.PosSlotDecoder(lenState), 6, o.range_dec, posSlot);
    if posSlot < 4 then
      dist:= UInt32(posSlot);
      return;
    end if;
    numDirectBits := Natural(Shift_Right(UInt32(posSlot), 1) - 1);
    dist := Shift_Left(2 or (UInt32(posSlot) and 1), numDirectBits);
    if posSlot < kEndPosModelIndex then
      Bit_Tree_Reverse_Decode(
        o.PosDecoders(Unsigned(dist) - posSlot .. Last_PosDecoders),
        numDirectBits, o.range_dec, dist
      );
    else
      Decode_Direct_Bits(o.range_dec, numDirectBits - kNumAlignBits, deco);
      dist:= dist + Shift_Left(deco, kNumAlignBits);
      Bit_Tree_Reverse_Decode(o.AlignDecoder, kNumAlignBits, o.range_dec, dist);
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
    Init(o.len_decoder);
    Init(o.rep_len_decoder);
  end Init;

  procedure Decode_Contents(o: in out LZMA_Decoder_Info; res: out LZMA_Result) is
    rep0, rep1, rep2, rep3 : UInt32 := 0;
    state : State_range := 0;
    pos_state: State_range;
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
      state := Update_State_Literal(state);
      o.unpackSize:= o.unpackSize - 1;
    end Process_Litteral;

    procedure Process_Distance_and_Length is
    pragma Inline(Process_Distance_and_Length);
      len: Unsigned;
      isError: Boolean;
      dist: UInt32;
      bit_a, bit_b, bit_c, bit_d, bit_e: Unsigned;
      kMatchMinLen : constant := 2;
    begin
      Decode_Bit(o.range_dec, o.IsRep(state), bit_a);
      if bit_a /= 0 then
        if o.unpackSize = 0 and then o.unpackSizeDefined then
          Raise_Exception(
            LZMA_Error'Identity,
            "Decoded data will exceed expected data size (in Process_Distance_and_Length, #1)"
          );
        end if;
        if Is_Empty(o.out_win) then
          Raise_Exception(
            LZMA_Error'Identity,
            "Output window buffer is empty (in Process_Distance_and_Length)"
          );
        end if;
        Decode_Bit(o.range_dec, o.IsRepG0(state), bit_b);
        if bit_b = 0 then
          Decode_Bit(o.range_dec, o.IsRep0Long(state * kNumPosBitsMax_Count + pos_state), bit_c);
          if bit_c = 0 then
            state := Update_State_ShortRep(state);
            Put_Byte(o.out_win, Get_Byte(o.out_win, rep0 + 1));
            o.unpackSize:= o.unpackSize - 1;
            return;  -- GdM: this way, we go to the next iteration (C++: continue)
          end if;
        else
          Decode_Bit(o.range_dec, o.IsRepG1(state), bit_d);
          if bit_d = 0 then
            dist := rep1;
          else
            Decode_Bit(o.range_dec, o.IsRepG2(state), bit_e);
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
        Decode(o.rep_len_decoder, o.range_dec, pos_state, len);
        state := Update_State_Rep(state);
      else
        rep3 := rep2;
        rep2 := rep1;
        rep1 := rep0;
        Decode(o.len_decoder, o.range_dec, pos_state, len);
        state := Update_State_Match(state);
        DecodeDistance(o, len, rep0);
        if rep0 = 16#FFFF_FFFF# then
          if Is_Finished_OK(o.range_dec) then
            raise Marker_exit;
          else
            Raise_Exception(
              LZMA_Error'Identity,
              "Range decoder not finished on EOS marker (in Process_Distance_and_Length)"
            );
          end if;
        end if;
        if (o.unpackSize = 0 and then o.unpackSizeDefined) or
            rep0 >= o.dictSize or not Check_Distance(o.out_win, rep0)
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
      Copy_Match(o.out_win, rep0 + 1, len); -- The LZ distance/length copy happens here.
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
    Init(o.range_dec);
    loop
      if o.unpackSize = 0
        and then (o.unpackSizeDefined and not o.markerIsMandatory)
        and then Is_Finished_OK(o.range_dec)
      then
        res:= LZMA_finished_without_marker;
        return;
      end if;
      pos_state := State_range(UInt32(o.out_win.total_pos) and o.pos_bits_mask);
      Decode_Bit(o.range_dec, o.IsMatch(state * kNumPosBitsMax_Count + pos_state), bit_choice);
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
    return o.range_dec.corrupted;
  end;

end LZMA_Decoding;
