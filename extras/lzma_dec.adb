with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Interfaces;                        use Interfaces;

procedure LZMA_Dec is
-- **c++**  /* LzmaSpec.c -- LZMA Reference Decoder
-- **c++**  2013-07-28 : Igor Pavlov : Public domain */

  subtype Byte is Unsigned_8;
  subtype UInt16 is Unsigned_16;
  subtype UInt32 is Unsigned_32;
  subtype UInt64 is Long_Long_Integer;    -- !! Unsigned_64 if provided
  type Unsigned is new Natural; -- unsigned integer, at least 16 bits in size
  
  f_in, f_out: Ada.Streams.Stream_IO.File_Type;

  function ReadByte return Byte is
    b: Byte;
  begin
    Byte'Read(Stream(f_in), b);
    return b;
  end;

  procedure WriteByte(b: Byte) is
  begin
    Byte'Write(Stream(f_out), b);
  end;

  type Byte_buffer is array(UInt32 range <>) of Byte;
  type p_Byte_buffer is access Byte_buffer;

  type COutWindow is record
    Buf      : p_Byte_buffer:= null;
    Pos      : UInt32;
    Size     : UInt32;
    IsFull   : Boolean;
    TotalPos : Unsigned;
  end record;

  procedure Create(o: in out COutWindow; dictSize: UInt32) is
  begin
    o.Buf      := new Byte_buffer(0..dictSize-1);
    o.Pos      := 0;
    o.Size     := dictSize;
    o.IsFull   := False;
    o.TotalPos := 0;
  end Create;

  procedure PutByte(o: in out COutWindow; b: Byte) is
  begin
    o.TotalPos := o.TotalPos + 1;
    o.Pos := o.Pos + 1;
    o.Buf(o.Pos):= b;
    if o.Pos = o.Size then
      o.Pos := 0;
      o.IsFull := True;
    end if;
    WriteByte(b);
  end PutByte;

  function GetByte(o: COutWindow; dist: UInt32) return Byte is
  begin
    if dist <= o.Pos then
      return o.Buf(o.Pos - dist);
    else
      return o.Buf(o.Size - dist + o.Pos);
    end if;
  end GetByte;

  procedure CopyMatch(o: in out COutWindow; dist: UInt32; len: Unsigned) is
  begin
    for i in 1..len loop
      PutByte(o, GetByte(o, dist));
    end loop;
  end CopyMatch;
  
  function CheckDistance(o: COutWindow; dist: UInt32) return Boolean is
  begin
    return dist <= o.Pos or o.IsFull;
  end CheckDistance;

  function IsEmpty(o: COutWindow) return Boolean is
  begin
    return o.Pos = 0 and not o.IsFull;
  end IsEmpty;

  kNumBitModelTotalBits : constant:= 11;
  kNumMoveBits          : constant:= 5;

  type CProb is new UInt16;
  type CProb_array is array(Unsigned range <>) of CProb;
  type p_CProb_array is access CProb_array;

  PROB_INIT_VAL : constant := (2 ** kNumBitModelTotalBits) / 2;

-- **c++**  #define INIT_PROBS(p) \
-- **c++**   { for (unsigned i = 0; i < sizeof(p) / sizeof(p[0]); i++) p[i] = PROB_INIT_VAL; }

  type CRangeDecoder is record
    RangeZ    : UInt32;
    Code      : UInt32;
    Corrupted : Boolean;
  end record;

  procedure Init(o: in out CRangeDecoder) is
  begin
    o.Corrupted := False;
    if ReadByte /= 0 then
      o.Corrupted := True;
    end if;
    o.RangeZ := 16#FFFF_FFFF#;
    o.Code   := 0;
    for i in 0..3 loop
      o.Code := Shift_Left(o.Code, 8) or UInt32(ReadByte);
    end loop;
    if o.Code = o.RangeZ then
      o.Corrupted := True;
    end if;
  end Init;

  function IsFinishedOK(o: CRangeDecoder) return Boolean is
  begin
    return o.Code = 0;
  end;

  kTopValue : constant := 2**24;

  procedure Normalize(o: in out CRangeDecoder) is
  begin
    if o.RangeZ < kTopValue then
      o.RangeZ := Shift_Left(o.RangeZ, 8);
      o.Code  := Shift_Left(o.Code, 8) or UInt32(ReadByte);
    end if;
  end Normalize;
 
  procedure DecodeDirectBits(o: in out CRangeDecoder; numBits : Unsigned; res: out UInt32) is
    t: UInt32;
  begin
    res := 0;
    for count in 1..numBits loop
      o.RangeZ := Shift_Right(o.RangeZ, 1);
      o.Code := o.Code - o.RangeZ;
      t := 0 - Shift_Right(o.Code, 31);
      o.Code := o.Code + (o.RangeZ and t);
      if o.Code = o.RangeZ then
        o.Corrupted := True;
      end if;
      Normalize(o);
      res := Shift_Left(res, 1);
      res := res + t + 1;
    end loop;
  end DecodeDirectBits;

  procedure DecodeBit(o: in out CRangeDecoder; prob: in out CProb; symbol: out UInt32) is
    v: UInt32 := UInt32(prob); -- unsigned in the C++ code
    bound: constant UInt32:= Shift_Right(o.RangeZ, kNumBitModelTotalBits) * v;
  begin
    if o.Code < bound then
      v:= v + Shift_Right( (Shift_Left(1, kNumBitModelTotalBits) - v) , kNumMoveBits);
      o.RangeZ := bound;
      symbol := 0;
    else
      v:= v - Shift_Right(v, kNumMoveBits);
      o.Code := o.Code - bound;
      o.RangeZ := o.RangeZ - bound;
      symbol := 1;
    end if;
    prob := CProb(v);
    Normalize(o);
  end DecodeBit;

  procedure BitTreeReverseDecode(prob: in out CProb_array; numBits : Unsigned; rc: in out CRangeDecoder; symbol: out UInt32) is
    m: UInt32 := 1;
    bit: UInt32;
  begin
    symbol := 0;
    for i in 0..numBits-1 loop
      DecodeBit(rc, prob(Unsigned(m)), bit);
      m := Shift_Left(m, 1);
      m := m + bit;
      symbol := symbol or Shift_Left(bit, Natural(i));
    end loop;
  end BitTreeReverseDecode;

  generic
    NumBits: Unsigned;
  package CBitTreeDecoder is
    subtype Probs is CProb_array(0 .. 2**Natural(NumBits) - 1);
    procedure Init(p: out Probs);
    procedure Decode(p: in out Probs; rc: in out CRangeDecoder; res: out Unsigned);
    procedure ReverseDecode(p: in out Probs; rc: in out CRangeDecoder; res: out UInt32);
  end;

  package body CBitTreeDecoder is

    procedure Init(p: out Probs) is
    begin
      p:= (others => PROB_INIT_VAL);
    end;

    procedure Decode(p: in out Probs; rc: in out CRangeDecoder; res: out Unsigned) is
      symbol: UInt32;
      m: Unsigned:= 1;
    begin
      for i in 0 .. NumBits-1 loop
        DecodeBit(rc, p(m),symbol);
        m:= m * 2 + Unsigned(symbol);
      end loop;
      res:= m - 2**Natural(NumBits);
    end Decode;

    procedure ReverseDecode(p: in out Probs; rc: in out CRangeDecoder; res: out UInt32) is
    begin
      BitTreeReverseDecode(p, NumBits, rc, res);
    end ReverseDecode;
    
  end CBitTreeDecoder;

  kNumPosBitsMax : constant := 4;

  kNumStates          : constant := 12;
  kNumLenToPosStates  : constant := 4;
  kNumAlignBits       : constant := 4;
  kStartPosModelIndex : constant := 4;
  kEndPosModelIndex   : constant := 14;
  kNumFullDistances   : constant := 2 ** (kEndPosModelIndex / 2);
  kMatchMinLen        : constant := 2;

  package BTD_3   is new CBitTreeDecoder(3);
  package BTD_6   is new CBitTreeDecoder(6);
  package BTD_8   is new CBitTreeDecoder(8);
  package BTD_NAB is new CBitTreeDecoder(kNumAlignBits);

  type LM_Coder_Probs is array(Unsigned'(0) .. 2**kNumPosBitsMax - 1) of BTD_3.Probs;

  type CLenDecoder is record
    Choice    : CProb;
    Choice2   : CProb;
    LowCoder  : LM_Coder_Probs;
    MidCoder  : LM_Coder_Probs;
    HighCoder : BTD_8.Probs;
  end record;

  use BTD_3, BTD_6, BTD_8, BTD_NAB;

  procedure Init(o: in out CLenDecoder) is
  begin
    o.Choice  := PROB_INIT_VAL;
    o.Choice2 := PROB_INIT_VAL;
    BTD_8.Init(o.HighCoder);
    for i in LM_Coder_Probs'Range loop
      BTD_3.Init(o.LowCoder(i));
      BTD_3.Init(o.MidCoder(i));
    end loop;
  end Init;

  procedure Decode(o: in out CLenDecoder; rc: in out CRangeDecoder; posState: Unsigned; res: out Unsigned) is
    symbol: UInt32;
  begin
    DecodeBit(rc, o.Choice, symbol);
    if symbol = 0 then
      BTD_3.Decode(o.LowCoder(posState), rc, res);
      return;
    end if;
    DecodeBit(rc, o.Choice2, symbol);
    if symbol = 0 then
      BTD_3.Decode(o.MidCoder(posState), rc, res);
      res:= res + 8;
      return;
    end if;
    BTD_8.Decode(o.HighCoder, rc, res);
    res:= res + 16;
  end Decode;

  type State_range is range 0..kNumStates-1;
  type Transition is array(State_range) of State_range;

  UpdateState_Literal  : constant Transition:= (0, 0, 0, 0, 1, 2, 3, 4,  5,  6,   4, 5);
  UpdateState_Match    : constant Transition:= (7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10);
  UpdateState_Rep      : constant Transition:= (8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11);
  UpdateState_ShortRep : constant Transition:= (9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11);

  LZMA_DIC_MIN : constant := 2 ** 12;

  type CLzmaDecoder is record
    RangeDec             : CRangeDecoder;
    OutWindow            : COutWindow;
    markerIsMandatory    : Boolean;
    lc, pb, lp           : Unsigned;
    dictSize             : UInt32;
    dictSizeInProperties : UInt32;
    LitProbs             : p_CProb_array;
  end record;

-- **c++**  class CLzmaDecoder
-- **c++**    void DecodeProperties(const Byte *properties)
-- **c++**    {
-- **c++**      unsigned d = properties[0];
-- **c++**      if (d >= (9 * 5 * 5))
-- **c++**        throw "Incorrect LZMA properties";
-- **c++**      lc = d % 9;
-- **c++**      d /= 9;
-- **c++**      pb = d / 5;
-- **c++**      lp = d % 5;
-- **c++**      dictSizeInProperties = 0;
-- **c++**      for (int i = 0; i < 4; i++)
-- **c++**        dictSizeInProperties |= (UInt32)properties[i + 1] << (8 * i);
-- **c++**      dictSize = dictSizeInProperties;
-- **c++**      if (dictSize < LZMA_DIC_MIN)
-- **c++**        dictSize = LZMA_DIC_MIN;
-- **c++**    }

-- **c++**    CLzmaDecoder(): LitProbs(NULL) {}
-- **c++**    ~CLzmaDecoder() { delete []LitProbs; }

  procedure CreateLiterals(o: in out CLzmaDecoder) is
    length: constant UInt32:= Shift_Left(16#300#, Natural(o.lc + o.lp));
  begin
    o.LitProbs := new CProb_array(0..Unsigned(length-1));
  end CreateLiterals;

  procedure Create(o: in out CLzmaDecoder) is
  begin
    Create(o.OutWindow, o.dictSize);
    CreateLiterals(o);
  end Create;

-- **c++**    int Decode(bool unpackSizeDefined, UInt64 unpackSize);
-- **c++**    
-- **c++**  private:
-- **c++**  
  procedure InitLiterals(o: in out CLzmaDecoder) is
  begin
    o.LitProbs.all:= (others => PROB_INIT_VAL);
  end InitLiterals;

-- **c++**    void DecodeLiteral(unsigned state, UInt32 rep0)
-- **c++**    {
-- **c++**      unsigned prevByte = 0;
-- **c++**      if (!OutWindow.IsEmpty())
-- **c++**        prevByte = OutWindow.GetByte(1);
-- **c++**      
-- **c++**      unsigned symbol = 1;
-- **c++**      unsigned litState = ((OutWindow.TotalPos & ((1 << lp) - 1)) << lc) + (prevByte >> (8 - lc));
-- **c++**      CProb *probs = &LitProbs[(UInt32)0x300 * litState];
-- **c++**      
-- **c++**      if (state >= 7)
-- **c++**      {
-- **c++**        unsigned matchByte = OutWindow.GetByte(rep0 + 1);
-- **c++**        do
-- **c++**        {
-- **c++**          unsigned matchBit = (matchByte >> 7) & 1;
-- **c++**          matchByte <<= 1;
-- **c++**          unsigned bit = RangeDec.DecodeBit(&probs[((1 + matchBit) << 8) + symbol]);
-- **c++**          symbol = (symbol << 1) | bit;
-- **c++**          if (matchBit != bit)
-- **c++**            break;
-- **c++**        }
-- **c++**        while (symbol < 0x100);
-- **c++**      }
-- **c++**      while (symbol < 0x100)
-- **c++**        symbol = (symbol << 1) | RangeDec.DecodeBit(&probs[symbol]);
-- **c++**      OutWindow.PutByte((Byte)(symbol - 0x100));
-- **c++**    }
-- **c++**  
-- **c++**    CBitTreeDecoder<6> PosSlotDecoder[kNumLenToPosStates];
-- **c++**    CBitTreeDecoder<kNumAlignBits> AlignDecoder;
-- **c++**    CProb PosDecoders[1 + kNumFullDistances - kEndPosModelIndex];
-- **c++**    
-- **c++**    void InitDist()
-- **c++**    {
-- **c++**      for (unsigned i = 0; i < kNumLenToPosStates; i++)
-- **c++**        PosSlotDecoder[i].Init();
-- **c++**      AlignDecoder.Init();
-- **c++**      INIT_PROBS(PosDecoders);
-- **c++**    }
-- **c++**    
-- **c++**    unsigned DecodeDistance(unsigned len)
-- **c++**    {
-- **c++**      unsigned lenState = len;
-- **c++**      if (lenState > kNumLenToPosStates - 1)
-- **c++**        lenState = kNumLenToPosStates - 1;
-- **c++**      
-- **c++**      unsigned posSlot = PosSlotDecoder[lenState].Decode(&RangeDec);
-- **c++**      if (posSlot < 4)
-- **c++**        return posSlot;
-- **c++**      
-- **c++**      unsigned numDirectBits = (unsigned)((posSlot >> 1) - 1);
-- **c++**      UInt32 dist = ((2 | (posSlot & 1)) << numDirectBits);
-- **c++**      if (posSlot < kEndPosModelIndex)
-- **c++**        dist += BitTreeReverseDecode(PosDecoders + dist - posSlot, numDirectBits, &RangeDec);
-- **c++**      else
-- **c++**      {
-- **c++**        dist += RangeDec.DecodeDirectBits(numDirectBits - kNumAlignBits) << kNumAlignBits;
-- **c++**        dist += AlignDecoder.ReverseDecode(&RangeDec);
-- **c++**      }
-- **c++**      return dist;
-- **c++**    }
-- **c++**  
-- **c++**    CProb IsMatch[kNumStates << kNumPosBitsMax];
-- **c++**    CProb IsRep[kNumStates];
-- **c++**    CProb IsRepG0[kNumStates];
-- **c++**    CProb IsRepG1[kNumStates];
-- **c++**    CProb IsRepG2[kNumStates];
-- **c++**    CProb IsRep0Long[kNumStates << kNumPosBitsMax];
-- **c++**  
-- **c++**    CLenDecoder LenDecoder;
-- **c++**    CLenDecoder RepLenDecoder;
-- **c++**  
-- **c++**    void Init()
-- **c++**    {
-- **c++**      InitLiterals();
-- **c++**      InitDist();
-- **c++**  
-- **c++**      INIT_PROBS(IsMatch);
-- **c++**      INIT_PROBS(IsRep);
-- **c++**      INIT_PROBS(IsRepG0);
-- **c++**      INIT_PROBS(IsRepG1);
-- **c++**      INIT_PROBS(IsRepG2);
-- **c++**      INIT_PROBS(IsRep0Long);
-- **c++**  
-- **c++**      LenDecoder.Init();
-- **c++**      RepLenDecoder.Init();
-- **c++**    }
-- **c++**  };
-- **c++**      
-- **c++**  
-- **c++**  #define LZMA_RES_ERROR                   0
-- **c++**  #define LZMA_RES_FINISHED_WITH_MARKER    1
-- **c++**  #define LZMA_RES_FINISHED_WITHOUT_MARKER 2
-- **c++**  
-- **c++**  int CLzmaDecoder::Decode(bool unpackSizeDefined, UInt64 unpackSize)
-- **c++**  {
-- **c++**    Init();
-- **c++**    RangeDec.Init();
-- **c++**  
-- **c++**    UInt32 rep0 = 0, rep1 = 0, rep2 = 0, rep3 = 0;
-- **c++**    unsigned state = 0;
-- **c++**    
-- **c++**    for (;;)
-- **c++**    {
-- **c++**      if (unpackSizeDefined && unpackSize == 0 && !markerIsMandatory)
-- **c++**        if (RangeDec.IsFinishedOK())
-- **c++**          return LZMA_RES_FINISHED_WITHOUT_MARKER;
-- **c++**  
-- **c++**      unsigned posState = OutWindow.TotalPos & ((1 << pb) - 1);
-- **c++**  
-- **c++**      if (RangeDec.DecodeBit(&IsMatch[(state << kNumPosBitsMax) + posState]) == 0)
-- **c++**      {
-- **c++**        if (unpackSizeDefined && unpackSize == 0)
-- **c++**          return LZMA_RES_ERROR;
-- **c++**        DecodeLiteral(state, rep0);
-- **c++**        state = UpdateState_Literal(state);
-- **c++**        unpackSize--;
-- **c++**        continue;
-- **c++**      }
-- **c++**      
-- **c++**      unsigned len;
-- **c++**      
-- **c++**      if (RangeDec.DecodeBit(&IsRep[state]) != 0)
-- **c++**      {
-- **c++**        if (unpackSizeDefined && unpackSize == 0)
-- **c++**          return LZMA_RES_ERROR;
-- **c++**        if (OutWindow.IsEmpty())
-- **c++**          return LZMA_RES_ERROR;
-- **c++**        if (RangeDec.DecodeBit(&IsRepG0[state]) == 0)
-- **c++**        {
-- **c++**          if (RangeDec.DecodeBit(&IsRep0Long[(state << kNumPosBitsMax) + posState]) == 0)
-- **c++**          {
-- **c++**            state = UpdateState_ShortRep(state);
-- **c++**            OutWindow.PutByte(OutWindow.GetByte(rep0 + 1));
-- **c++**            unpackSize--;
-- **c++**            continue;
-- **c++**          }
-- **c++**        }
-- **c++**        else
-- **c++**        {
-- **c++**          UInt32 dist;
-- **c++**          if (RangeDec.DecodeBit(&IsRepG1[state]) == 0)
-- **c++**            dist = rep1;
-- **c++**          else
-- **c++**          {
-- **c++**            if (RangeDec.DecodeBit(&IsRepG2[state]) == 0)
-- **c++**              dist = rep2;
-- **c++**            else
-- **c++**            {
-- **c++**              dist = rep3;
-- **c++**              rep3 = rep2;
-- **c++**            }
-- **c++**            rep2 = rep1;
-- **c++**          }
-- **c++**          rep1 = rep0;
-- **c++**          rep0 = dist;
-- **c++**        }
-- **c++**        len = RepLenDecoder.Decode(&RangeDec, posState);
-- **c++**        state = UpdateState_Rep(state);
-- **c++**      }
-- **c++**      else
-- **c++**      {
-- **c++**        rep3 = rep2;
-- **c++**        rep2 = rep1;
-- **c++**        rep1 = rep0;
-- **c++**        len = LenDecoder.Decode(&RangeDec, posState);
-- **c++**        state = UpdateState_Match(state);
-- **c++**        rep0 = DecodeDistance(len);
-- **c++**        if (rep0 == 0xFFFFFFFF)
-- **c++**          return RangeDec.IsFinishedOK() ?
-- **c++**              LZMA_RES_FINISHED_WITH_MARKER :
-- **c++**              LZMA_RES_ERROR;
-- **c++**  
-- **c++**        if (unpackSizeDefined && unpackSize == 0)
-- **c++**          return LZMA_RES_ERROR;
-- **c++**        if (rep0 >= dictSize || !OutWindow.CheckDistance(rep0))
-- **c++**          return LZMA_RES_ERROR;
-- **c++**      }
-- **c++**      len += kMatchMinLen;
-- **c++**      bool isError = false;
-- **c++**      if (unpackSizeDefined && unpackSize < len)
-- **c++**      {
-- **c++**        len = (unsigned)unpackSize;
-- **c++**        isError = true;
-- **c++**      }
-- **c++**      OutWindow.CopyMatch(rep0 + 1, len);
-- **c++**      unpackSize -= len;
-- **c++**      if (isError)
-- **c++**        return LZMA_RES_ERROR;
-- **c++**    }
-- **c++**  }
-- **c++**  
-- **c++**  static void Print(const char *s)
-- **c++**  {
-- **c++**    fputs(s, stdout);
-- **c++**  }
-- **c++**  
-- **c++**  static void PrintError(const char *s)
-- **c++**  {
-- **c++**    fputs(s, stderr);
-- **c++**  }
-- **c++**  
-- **c++**  
-- **c++**  #define CONVERT_INT_TO_STR(charType, tempSize) \
-- **c++**  
-- **c++**  void ConvertUInt64ToString(UInt64 val, char *s)
-- **c++**  {
-- **c++**    char temp[32];
-- **c++**    unsigned i = 0;
-- **c++**    while (val >= 10)
-- **c++**    {
-- **c++**      temp[i++] = (char)('0' + (unsigned)(val % 10));
-- **c++**      val /= 10;
-- **c++**    }
-- **c++**    *s++ = (char)('0' + (unsigned)val);
-- **c++**    while (i != 0)
-- **c++**    {
-- **c++**      i--;
-- **c++**      *s++ = temp[i];
-- **c++**    }
-- **c++**    *s = 0;
-- **c++**  }
-- **c++**  
-- **c++**  void PrintUInt64(const char *title, UInt64 v)
-- **c++**  {
-- **c++**    Print(title);
-- **c++**    Print(" : ");
-- **c++**    char s[32];
-- **c++**    ConvertUInt64ToString(v, s);
-- **c++**    Print(s);
-- **c++**    Print(" bytes \n");
-- **c++**  }

  begin
    New_Line;
    Put_Line("LZMA Reference Decoder 9.31 : Igor Pavlov : Public domain : 2013-02-06");
    if Argument_Count = 0 then
      Put_Line("Use: lzma_dec a.lzma outfile");
      return;
    elsif Argument_Count /= 2 then
      Put_Line("You must specify two parameters");
      return;
    end if;
    Open(f_in, In_File, Argument(1));
    
-- **c++**    CInputStream inStream;
-- **c++**    inStream.File = fopen(args[1], "rb");
-- **c++**    inStream.Init();
-- **c++**    if (inStream.File == 0)
-- **c++**      throw "Can't open input file";
-- **c++**  
-- **c++**    CLzmaDecoder lzmaDecoder;
    Create(f_out,Out_File, Argument(2));
-- **c++**    lzmaDecoder.OutWindow.OutStream.File = fopen(args[2], "wb+");
-- **c++**    lzmaDecoder.OutWindow.OutStream.Init();
-- **c++**    if (inStream.File == 0)
-- **c++**      throw "Can't open output file";
-- **c++**  
-- **c++**    Byte header[13];
-- **c++**    int i;
-- **c++**    for (i = 0; i < 13; i++)
-- **c++**      header[i] = inStream.ReadByte();
-- **c++**  
-- **c++**    lzmaDecoder.DecodeProperties(header);
-- **c++**  
-- **c++**    printf("\nlc=%d, lp=%d, pb=%d", lzmaDecoder.lc, lzmaDecoder.lp, lzmaDecoder.pb);
-- **c++**    printf("\nDictionary Size in properties = %u", lzmaDecoder.dictSizeInProperties);
-- **c++**    printf("\nDictionary Size for decoding  = %u", lzmaDecoder.dictSize);
-- **c++**  
-- **c++**    UInt64 unpackSize = 0;
-- **c++**    bool unpackSizeDefined = false;
-- **c++**    for (i = 0; i < 8; i++)
-- **c++**    {
-- **c++**      Byte b = header[5 + i];
-- **c++**      if (b != 0xFF)
-- **c++**        unpackSizeDefined = true;
-- **c++**      unpackSize |= (UInt64)b << (8 * i);
-- **c++**    }
-- **c++**  
-- **c++**    lzmaDecoder.markerIsMandatory = !unpackSizeDefined;
-- **c++**  
-- **c++**    Print("\n");
-- **c++**    if (unpackSizeDefined)
-- **c++**      PrintUInt64("Uncompressed Size", unpackSize);
-- **c++**    else
-- **c++**      Print("End marker is expected\n");
-- **c++**    lzmaDecoder.RangeDec.InStream = &inStream;
-- **c++**  
-- **c++**    Print("\n");
-- **c++**  
-- **c++**    lzmaDecoder.Create();
-- **c++**    // we support the streams that have uncompressed size and marker.
-- **c++**    int res = lzmaDecoder.Decode(unpackSizeDefined, unpackSize);
-- **c++**  
-- **c++**    PrintUInt64("Read    ", inStream.Processed);
-- **c++**    PrintUInt64("Written ", lzmaDecoder.OutWindow.OutStream.Processed);
-- **c++**  
-- **c++**    if (res == LZMA_RES_ERROR)
-- **c++**      throw "LZMA decoding error";
-- **c++**    else if (res == LZMA_RES_FINISHED_WITHOUT_MARKER)
-- **c++**      Print("Finished without end marker");
-- **c++**    else if (res == LZMA_RES_FINISHED_WITH_MARKER)
-- **c++**    {
-- **c++**      if (unpackSizeDefined)
-- **c++**      {
-- **c++**        if (lzmaDecoder.OutWindow.OutStream.Processed != unpackSize)
-- **c++**          throw "Finished with end marker before than specified size";
-- **c++**        Print("Warning: ");
-- **c++**      }
-- **c++**      Print("Finished with end marker");
-- **c++**    }
-- **c++**    else
-- **c++**      throw "Internal Error";
-- **c++**  
-- **c++**    Print("\n");
-- **c++**    
-- **c++**    if (lzmaDecoder.RangeDec.Corrupted)
-- **c++**    {
-- **c++**      Print("\nWarning: LZMA stream is corrupted\n");
-- **c++**    }
-- **c++**  
-- **c++**    return 0;
-- **c++**  }
-- **c++**  
-- **c++**  
-- **c++**  int
-- **c++**    #ifdef _MSC_VER
-- **c++**      __cdecl
-- **c++**    #endif
-- **c++**  main(int numArgs, const char *args[])
-- **c++**  {
-- **c++**    try { return main2(numArgs, args); }
-- **c++**    catch (const char *s)
-- **c++**    {
-- **c++**      PrintError("\nError:\n");
-- **c++**      PrintError(s);
-- **c++**      PrintError("\n");
-- **c++**      return 1;
-- **c++**    }
-- **c++**    catch(...)
-- **c++**    {
-- **c++**      PrintError("\nError\n");
-- **c++**      return 1;
-- **c++**    }
-- **c++**  }
-- **c++**  
end LZMA_Dec;
