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

  type Range_Decoder is record
    range_z   : UInt32;
    code      : UInt32;
    corrupted : Boolean;
  end record;

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

  kNumBitModelTotalBits : constant:= 11;
  kNumMoveBits          : constant:= 5;
  kNumBitModel_Count    : constant:= 2 ** kNumBitModelTotalBits;

  PROB_INIT_VAL : constant := (2 ** kNumBitModelTotalBits) / 2;

  procedure Init(o: in out Length_Decoder) is
  begin
    o.choice     := PROB_INIT_VAL;
    o.choice_2   := PROB_INIT_VAL;
    o.high_coder := (others => PROB_INIT_VAL);
    o.low_coder  := (others => (others => PROB_INIT_VAL));
    o.mid_coder  := (others => (others => PROB_INIT_VAL));
  end Init;

  LZMA_DIC_MIN : constant := 2 ** 12;

  procedure Finalize_Manually(o: in out LZMA_Decoder_Info) is
    procedure Dispose is new Ada.Unchecked_Deallocation(CProb_array, p_CProb_array);
  begin
    Dispose(o.LitProbs);
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
    o.pb := PB_range(d / 5);
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
    o.LitProbs := new CProb_array(0..length-1); -- Literals
  end Create_Large_Arrays;

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
    subtype State_range is Unsigned range 0..kNumStates-1;
    state : State_range := 0;
    rep0, rep1, rep2, rep3 : UInt32 := 0;
    pos_state: State_range;
    -- Local copies of invariant properties.
    unpack_size_def: constant Boolean:= o.unpackSizeDefined;
    literal_pos_mask: constant UInt32:= 2 ** o.lp - 1;
    lc: constant LC_range:= o.lc;
    --
    use type BIO.Count;
    Marker_exit: exception;
    out_win : Out_Window;
    -- Local range decoder
    loc_range_dec: Range_Decoder;
    --
    type Transition is array(State_range) of State_range;

    Update_State_Literal  : constant Transition:= (0, 0, 0, 0, 1, 2, 3,  4,  5,  6,  4,  5);
    Update_State_Match    : constant Transition:= (7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10);
    Update_State_Rep      : constant Transition:= (8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11);
    Update_State_ShortRep : constant Transition:= (9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11);
    --
    procedure Normalize_Q is
    pragma Inline(Normalize_Q);
      kTopValue : constant := 2**24;
    begin
      if loc_range_dec.range_z < kTopValue then
        loc_range_dec.range_z := Shift_Left(loc_range_dec.range_z, 8);
        loc_range_dec.code  := Shift_Left(loc_range_dec.code, 8) or UInt32(Read_Byte);
      end if;
    end Normalize_Q;

    procedure Decode_Bit_Q(prob: in out CProb; symbol: out Unsigned) is
    pragma Inline(Decode_Bit_Q);
      prob_l: constant CProb:= prob; -- Local copy
      bound: constant UInt32:= Shift_Right(loc_range_dec.range_z, kNumBitModelTotalBits) * prob_l;
    begin
      if loc_range_dec.code < bound then
        prob:= prob_l + Shift_Right(kNumBitModel_Count - prob_l, kNumMoveBits);
        loc_range_dec.range_z := bound;
        Normalize_Q;
        symbol := 0;
      else
        prob:= prob_l - Shift_Right(prob_l, kNumMoveBits);
        loc_range_dec.code := loc_range_dec.code - bound;
        loc_range_dec.range_z := loc_range_dec.range_z - bound;
        Normalize_Q;
        symbol := 1;
      end if;
    end Decode_Bit_Q;

    function Is_Empty return Boolean is
    pragma Inline(Is_Empty);
    begin
      return out_win.pos = 0 and then not out_win.is_full;
    end;

    procedure Put_Byte_Q(b: Byte) is
    pragma Inline(Put_Byte_Q);
    begin
      out_win.total_pos := out_win.total_pos + 1;
      out_win.buf(out_win.pos):= b;
      out_win.pos := out_win.pos + 1;
      if out_win.pos = out_win.size then
        out_win.pos := 0;
        out_win.is_full := True;
      end if;
      Write_Byte(b);
    end Put_Byte_Q;

    function Get_Byte_Q(dist: UInt32) return Byte is
    pragma Inline(Get_Byte_Q);
    begin
      if dist <= out_win.pos then
        return out_win.buf(out_win.pos - dist);
      else
        return out_win.buf(out_win.size - dist + out_win.pos);
      end if;
    end Get_Byte_Q;
  
    procedure Process_Litteral is
    pragma Inline(Process_Litteral);
      prevByte     : Byte:= 0;
      symbol       : Unsigned:= 1;
      lit_state    : Unsigned;
      probs_idx    : Unsigned;
      matchByte    : UInt32;
      matchBit     : UInt32;
      bit_a, bit_b : Unsigned;
    begin
      if o.unpackSize = 0 and then unpack_size_def then
        Raise_Exception(
          LZMA_Error'Identity,
          "Decoded data will exceed expected data size (Process_Litteral)"
        );
      end if;
      --
      if not Is_Empty then
        prevByte := Get_Byte_Q(1);
      end if;
      lit_state :=
        Unsigned(
          Shift_Left(UInt32(out_win.total_pos) and literal_pos_mask, lc) +
          Shift_Right(UInt32(prevByte), 8 - lc)
        );
      probs_idx:= 16#300# * lit_state;
      if state >= 7 then
        matchByte := UInt32(Get_Byte_Q(rep0 + 1));
        loop
          matchBit  := Shift_Right(matchByte, 7) and 1;
          matchByte := matchByte + matchByte;
          Decode_Bit_Q(
            o.LitProbs(probs_idx + Unsigned(Shift_Left(1 + matchBit, 8)) + symbol),
            bit_a
          );
          symbol := (symbol + symbol) or bit_a;
          exit when (Unsigned(matchBit) /= bit_a) or else (symbol >= 16#100#);
        end loop;
      end if;
      while symbol < 16#100# loop
        Decode_Bit_Q(o.LitProbs(probs_idx + symbol), bit_b);
        symbol := (symbol + symbol) or bit_b;
      end loop;
      Put_Byte_Q(Byte(symbol - 16#100#)); -- The output of a simple literal happens here.
      --
      state := Update_State_Literal(state);
      o.unpackSize:= o.unpackSize - 1;
    end Process_Litteral;

    dict_size : constant UInt32:= o.dictSize;

    function Is_Finished_OK return Boolean is
    pragma Inline(Is_Finished_OK);
    begin
      return loc_range_dec.code = 0;
    end;

    procedure Process_Distance_and_Length is
    pragma Inline(Process_Distance_and_Length);
      --
      procedure Bit_Tree_Decode(
        prob     : in out CProb_array;
        num_bits :        Positive;
        m        :    out Unsigned)
      is
      pragma Inline(Bit_Tree_Decode);
        bit: Unsigned;
      begin
        m:= 1;
        for count in reverse 1..num_bits loop
          Decode_Bit_Q(prob(m + prob'First), bit);
          m:= m + m + bit;
        end loop;
        m:= m - 2**num_bits;
      end Bit_Tree_Decode;
      --
      len: Unsigned;
      --
      procedure Copy_Match_Q(dist: UInt32) is
      pragma Inline(Copy_Match_Q);
      begin
        for count in reverse 1..len loop
          Put_Byte_Q(Get_Byte_Q(dist));
        end loop;
      end Copy_Match_Q;
      --
      procedure Decode_Distance(dist: out UInt32) is
      pragma Inline(Decode_Distance);
        --
        decode_direct: UInt32;
        --
        procedure Decode_Direct_Bits(num_bits : Natural) is
        pragma Inline(Decode_Direct_Bits);
          t: UInt32;
        begin
          decode_direct := 0;
          for count in reverse 1..num_bits loop
            loc_range_dec.range_z := Shift_Right(loc_range_dec.range_z, 1);
            loc_range_dec.code := loc_range_dec.code - loc_range_dec.range_z;
            t := - Shift_Right(loc_range_dec.code, 31);
            loc_range_dec.code := loc_range_dec.code + (loc_range_dec.range_z and t);
            if loc_range_dec.code = loc_range_dec.range_z then
              loc_range_dec.corrupted := True;
            end if;
            Normalize_Q;
            decode_direct := decode_direct + decode_direct + t + 1;
          end loop;
        end Decode_Direct_Bits;
        --
        procedure Bit_Tree_Reverse_Decode(prob: in out CProb_array; num_bits: in Natural) is
        pragma Inline(Bit_Tree_Reverse_Decode);
          m: Unsigned := 1;
          bit: Unsigned;
        begin
          for i in 0..num_bits-1 loop
            Decode_Bit_Q(prob(m + prob'First), bit);
            m := m + m + bit;
            dist := dist or Shift_Left(UInt32(bit), i);
          end loop;
        end Bit_Tree_Reverse_Decode;
        --
        len_state     : Unsigned := len;
        posSlot       : Unsigned;
        numDirectBits : Natural;
        --
      begin -- Decode_Distance
        if len_state > kNumLenToPosStates - 1 then
          len_state := kNumLenToPosStates - 1;
        end if;
        Bit_Tree_Decode(o.PosSlotDecoder(len_state), 6, posSlot);
        if posSlot < 4 then
          dist:= UInt32(posSlot);
          return;
        end if;
        numDirectBits := Natural(Shift_Right(UInt32(posSlot), 1) - 1);
        dist := Shift_Left(2 or (UInt32(posSlot) and 1), numDirectBits);
        if posSlot < kEndPosModelIndex then
          Bit_Tree_Reverse_Decode(
            o.PosDecoders(Unsigned(dist) - posSlot .. Last_PosDecoders),
            numDirectBits
          );
        else
          Decode_Direct_Bits(numDirectBits - kNumAlignBits);
          dist:= dist + Shift_Left(decode_direct, kNumAlignBits);
          Bit_Tree_Reverse_Decode(o.AlignDecoder, kNumAlignBits);
        end if;
      end Decode_Distance;
      --
      procedure Decode_Length(o: in out Length_Decoder) is
      pragma Inline(Decode_Length);
        bit_a, bit_b: Unsigned;
      begin
        Decode_Bit_Q(o.choice, bit_a);
        if bit_a = 0 then
          Bit_Tree_Decode(o.low_coder(pos_state), 3, len);
          return;
        end if;
        Decode_Bit_Q(o.choice_2, bit_b);
        if bit_b = 0 then
          Bit_Tree_Decode(o.mid_coder(pos_state), 3, len);
          len:= len + 8;
          return;
        end if;
        Bit_Tree_Decode(o.high_coder, 8, len);
        len:= len + 16;
      end Decode_Length;
      --
      function Check_Distance return Boolean is
      pragma Inline(Check_Distance);
      begin
        return rep0 <= out_win.pos or out_win.is_full;
      end;
      --
      isError: Boolean;
      dist: UInt32;
      bit_a, bit_b, bit_c, bit_d, bit_e: Unsigned;
      kMatchMinLen : constant := 2;
      --
    begin -- Process_Distance_and_Length
      Decode_Bit_Q(o.IsRep(state), bit_a);
      if bit_a /= 0 then
        if o.unpackSize = 0 and then unpack_size_def then
          Raise_Exception(
            LZMA_Error'Identity,
            "Decoded data will exceed expected data size (in Process_Distance_and_Length, #1)"
          );
        end if;
        if Is_Empty then
          Raise_Exception(
            LZMA_Error'Identity,
            "Output window buffer is empty (in Process_Distance_and_Length)"
          );
        end if;
        Decode_Bit_Q(o.IsRepG0(state), bit_b);
        if bit_b = 0 then
          Decode_Bit_Q(o.IsRep0Long(state * kNumPosBitsMax_Count + pos_state), bit_c);
          if bit_c = 0 then
            state := Update_State_ShortRep(state);
            Put_Byte_Q(Get_Byte_Q(rep0 + 1));
            o.unpackSize:= o.unpackSize - 1;
            return;  -- GdM: this way, we go to the next iteration (C++: continue)
          end if;
        else
          Decode_Bit_Q(o.IsRepG1(state), bit_d);
          if bit_d = 0 then
            dist := rep1;
          else
            Decode_Bit_Q(o.IsRepG2(state), bit_e);
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
        Decode_Length(o.rep_len_decoder);
        state := Update_State_Rep(state);
      else
        rep3 := rep2;
        rep2 := rep1;
        rep1 := rep0;
        Decode_Length(o.len_decoder);
        state := Update_State_Match(state);
        Decode_Distance(dist => rep0);
        if rep0 = 16#FFFF_FFFF# then
          if Is_Finished_OK then
            raise Marker_exit;
          else
            Raise_Exception(
              LZMA_Error'Identity,
              "Range decoder not finished on EOS marker (in Process_Distance_and_Length)"
            );
          end if;
        end if;
        if (o.unpackSize = 0 and then unpack_size_def) or
            rep0 >= dict_size or not Check_Distance
        then
          Raise_Exception(
            LZMA_Error'Identity,
            "Decoded data will exceed expected data size (in Process_Distance_and_Length, #2)"
          );
        end if;
      end if;
      len := len + kMatchMinLen;
      isError := False;
      if o.unpackSize < Data_Bytes_Count(len) and then unpack_size_def then
        len := Unsigned(o.unpackSize);
        isError := True;
      end if;
      -- The LZ distance/length copy happens here.
      Copy_Match(out_win, rep0 + 1, len); 
      -- Copy_Match_Q(rep0 + 1); 
      o.unpackSize:= o.unpackSize - Data_Bytes_Count(len);
      if isError then
        Raise_Exception(
          LZMA_Error'Identity,
          "Decoded data will exceed expected data size (in Process_Distance_and_Length, #3)"
        );
      end if;
    end Process_Distance_and_Length;

    bit_choice: Unsigned;
    pos_bits_mask : constant UInt32 := 2 ** o.pb - 1;
    size_defined_and_marker_not_mandatory: constant Boolean:=
      unpack_size_def and not o.markerIsMandatory;
    procedure Dispose is new Ada.Unchecked_Deallocation(Byte_buffer, p_Byte_buffer);

  begin
    Create(out_win, o.dictSize);
    Init(o);
    Init(loc_range_dec);
    loop
      if o.unpackSize = 0
        and then Is_Finished_OK
        and then size_defined_and_marker_not_mandatory
      then
        res:= LZMA_finished_without_marker;
        Dispose(out_win.buf);
        return;
      end if;
      pos_state := State_range(UInt32(out_win.total_pos) and pos_bits_mask);
      Decode_Bit_Q(o.IsMatch(state * kNumPosBitsMax_Count + pos_state), bit_choice);
      -- LZ decoding happens here: either we have a new literal in 1 byte, or we copy past data.
      if bit_choice = 0 then
        Process_Litteral;
      else
        Process_Distance_and_Length;
      end if;
    end loop;
  exception
    when Marker_exit =>
      res:= LZMA_finished_with_marker;
      Dispose(out_win.buf);
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
    return o.range_dec_corrupted;
  end;

end LZMA_Decoding;
