--  This corresponds +/- to ppmd7.c (Igor) and model.cpp (Dmitri)

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;

package body PPMd.Model is

  --  !! de-obfuscation: Exponential Escapes
  PPMD7_kExpEscape : constant array (0 .. 15) of Byte :=
    (25, 14, 9, 7, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2);

  --  !! de-obfuscation: Initial Binary Escapes
  kInitBinEsc : constant array (0 .. 7) of UInt16 :=
    (16#3CDD#, 16#1F3F#, 16#59BF#, 16#48F3#, 16#64A1#, 16#5ABC#, 16#6632#, 16#6051#);

  MAX_FREQ  : constant := 124;
  UNIT_SIZE : constant := 12;

  type CPpmd7_Node;

  type CPpmd7_Node_access is access CPpmd7_Node;
  subtype CPpmd7_Node_Ref is Big_mem_index;

  type CPpmd7_Node is record
    Stamp : UInt16;  --  /* must be at offset 0 as CPpmd7_Context::NumStats. Stamp=0 means free */
    NU    : UInt16;
    Next  : CPpmd7_Node_Ref; --  /* must be at offset >= 4 */
    Prev  : CPpmd7_Node_Ref;
  end record;

  procedure Ppmd7_Construct (p : in out CPpmd7) is
    k, m, step : Unsigned;
  begin
    p.Base := null;
    k := 0;
    for i in Unsigned'(0) .. PPMD_NUM_INDEXES - 1 loop
      if i >= 12 then
        step := 4;
      else
        step := i / 4 + 1;
      end if;
      loop
        p.Units2Indx (k) := Byte (i);
        k := k + 1;
        step := step - 1;
        exit when step = 0;
      end loop;
      p.Indx2Units (i) := Byte (k);
    end loop;
    p.NS2BSIndx :=
      (0       => 0,
       1       => 2,
       2 .. 10 => 4,
       others  => 6
      );
    p.NS2Indx (0 .. 2) := (0, 1, 2);
    m := 3;
    k := 1;
    for i in 3 .. 255 loop
      p.NS2Indx (i) := Byte (m);
      k := k - 1;
      if k = 0 then
        m := m + 1;
        k := m - 2;
      end if;
    end loop;
    p.HB2Flag :=
      (0      .. 16#3F# => 0,
       16#40# .. 16#FF# => 8
      );
  end Ppmd7_Construct;

  procedure Big_mem_free is
    new Ada.Unchecked_Deallocation (Big_mem_array, Big_mem_array_access);

  procedure Ppmd7_Free (p : in out CPpmd7) is
  begin
    Big_mem_free (p.Base);
    p.Size := 0;
  end Ppmd7_Free;

  procedure Ppmd7_Alloc (p : in out CPpmd7; size : UInt32) is
    size2 : UInt32;
  begin
    if p.Base /= null or else p.Size /= size then
      Ppmd7_Free (p);
      size2 := UNIT_SIZE;
      p.AlignOffset := 4 - (size and 3);
      p.Base        := new Big_mem_array (0 .. p.AlignOffset + size + size2 - 1);
      p.Size        := size;
    end if;
  end Ppmd7_Alloc;

  --  Put numbers with specific endianess as bytes:
  generic
    type Number is mod <>;
    size : Big_mem_index;
  function Intel_buffer (n : Number) return Big_mem_array;
  pragma Inline (Intel_buffer);

  function Intel_buffer (n : Number) return Big_mem_array is
    b : Big_mem_array (1 .. size);
    m : Number := n;
  begin
    for i in b'Range loop
      b (i) := Byte (m and 255);
      m := m / 256;
    end loop;
    return b;
  end Intel_buffer;

  function To_Intel_Bytes is new Intel_buffer (Big_mem_index, 4);

  function From_Intel_Bytes (m : Big_mem_array) return Big_mem_index is
  pragma Inline (From_Intel_Bytes);
  begin
    return Big_mem_index (m'First) +
           Big_mem_index (m'First + 1) * 16#1_00# +
           Big_mem_index (m'First + 2) * 16#1_00_00# +
           Big_mem_index (m'First + 3) * 16#1_00_00_00#;
  end From_Intel_Bytes;

  --  !!  tricky C code translated, there be translation bugs!

  procedure InsertNode (p : in out CPpmd7; node : Big_mem_index; indx : Unsigned) is
  begin
    p.Base (node .. node + 3) := To_Intel_Bytes (p.FreeList (indx));
    p.FreeList (indx) := node;
  end InsertNode;

  procedure RemoveNode (p : in out CPpmd7; indx : Unsigned; node : out Big_mem_index) is
    node_ref : Big_mem_index;
    old_free_idx : Big_mem_index := p.FreeList (indx);
  begin
    node_ref := From_Intel_Bytes (p.Base (old_free_idx .. old_free_idx + 3));
    p.FreeList (indx) := node_ref;
    node := From_Intel_Bytes (p.Base (node_ref .. node_ref + 3));
  end RemoveNode;

  --  Block management

  function U2B (nu : Unsigned) return Big_mem_index is
  pragma Inline (U2B);
  begin
    return Big_mem_index (nu) * UNIT_SIZE;
  end U2B;

  function U2I (p : CPpmd7; nu : Unsigned) return Unsigned is
  pragma Inline (U2I);
  begin
    return Unsigned (p.Units2Indx (nu - 1));
  end U2I;

  procedure SplitBlock (p : in out CPpmd7; ptr_0 : Big_mem_index; oldIndx, newIndx : Unsigned) is
    i, nu, k : Unsigned;
    ptr : Big_mem_index;
  begin
    --  !! perhaps convert before subtraction :
    nu := Unsigned (p.Indx2Units (oldIndx) - p.Indx2Units (newIndx));
    ptr := ptr_0 + U2B (Unsigned (p.Indx2Units (newIndx)));
    i := U2I (p, nu);
    if Unsigned (p.Indx2Units (i)) /= nu then
      i := i - 1;
      k := Unsigned (p.Indx2Units (i));
      InsertNode (p, ptr + U2B (k), nu - k - 1);
    end if;
    InsertNode (p, ptr, i);
  end SplitBlock;

  type Byte_access is access all Byte;

  procedure GlueFreeBlocks (p : in out CPpmd7) is
    function NNODE (ref : CPpmd7_Node_Ref) return CPpmd7_Node_access is
      pragma Inline (NNODE);
      function Convert is new Ada.Unchecked_Conversion (Byte_access, CPpmd7_Node_access);
    begin
      return Convert (p.Base (ref)'Access);
    end NNODE;
    head   : CPpmd7_Node_Ref := p.AlignOffset + p.Size;
    n      : CPpmd7_Node_Ref := head;
    next   : CPpmd7_Node_Ref;
    n_copy : CPpmd7_Node_Ref;
    node,
    node2  : CPpmd7_Node_access;
    nu16   : UInt16;
    nu32   : UInt32;
    nuuu, iu, k  : Unsigned;
  begin
    p.GlueCount := 255;
    --  /* create doubly-linked list of free blocks */
    for i in Free_list_array'Range loop
      nu16 := UInt16 (p.Indx2Units (i));
      next := p.FreeList (i);
      p.FreeList (i) := 0;
      while next /= 0 loop
        node := NNODE (next);
        node.Next := n;
        NNODE (n).Prev := next;
        n := next;
        --  !!!  next = *(const CPpmd7_Node_Ref *)node;
        node.Stamp := 0;
        node.NU := UInt16 (nu16);
      end loop;
    end loop;
    NNODE (head).Stamp := 1;
    NNODE (head).Next := n;
    NNODE (n).Prev := head;
    if p.LoUnit /= p.HiUnit then
      null;  -- !!! ((CPpmd7_Node *)p->LoUnit)->Stamp = 1;
    end if;

    --  /* Glue free blocks */
    while n /= head loop
      node := NNODE (n);
      nu32 := UInt32 (node.NU);
      loop
        node2 := NNODE (n + nu32);
        nu32 := nu32 + UInt32 (node2.NU);
        exit when node2.Stamp /= 0 or else nu32 >= 16#10000#;
        NNODE (node2.Prev).Next := node2.Next;
        NNODE (node2.Next).Prev := node2.Prev;
        node.NU := UInt16 (nu32);
      end loop;
      n := node.Next;
    end loop;

    --  /* Fill lists of free blocks */
    n := NNODE (head).Next;
    while n /= head loop
      node := NNODE (n);
      next := node.Next;
      nuuu := Unsigned (node.NU);
      n_copy := n;
      while nuuu > 128 loop
        InsertNode (p, n_copy, PPMD_NUM_INDEXES - 1);
        nuuu := nuuu - 128;
        n_copy := n_copy + 128;
      end loop;
      iu := U2I (p, nuuu);
      if Unsigned (p.Indx2Units (iu)) /= nuuu then
        iu := iu - 1;
        k := Unsigned (p.Indx2Units (iu));
        InsertNode (p, n_copy + Big_mem_index (k), nuuu - k - 1);
      end if;
      InsertNode (p, n_copy, iu);
      n := next;
    end loop;
  end GlueFreeBlocks;

  procedure AllocUnitsRare (p : in out CPpmd7; indx : Unsigned; node_ref : out Big_mem_index) is
    i : Unsigned;
    retVal : Big_mem_index;
    numBytes : UInt32;
  begin
    if p.GlueCount = 0 then
      GlueFreeBlocks (p);
      if p.FreeList (indx) /= 0 then
        RemoveNode (p, indx, node_ref);
        return;
      end if;
    end if;
    i := indx;
    loop
      i := i + 1;
      if i = PPMD_NUM_INDEXES then
        numBytes := U2B (Unsigned (p.Indx2Units (indx)));
        p.GlueCount := p.GlueCount - 1;
        if UInt32 (p.UnitsStart - p.Text) > numBytes then
          p.UnitsStart := p.UnitsStart - numBytes;
          node_ref := p.UnitsStart;
        else
          node_ref := 0;
        end if;
        return;
      end if;
      exit when p.FreeList (i) /= 0;
    end loop;
    RemoveNode (p, i, retVal);
    SplitBlock (p, retVal, i, indx);
    node_ref := retVal;
  end AllocUnitsRare;

  procedure AllocUnits (p : in out CPpmd7; indx : Unsigned; node_ref : out Big_mem_index) is
    retVal : Big_mem_index;
    numBytes : UInt32;
  begin
    if p.FreeList (indx) /= 0 then
      RemoveNode (p, indx, node_ref);
      return;
    end if;
    numBytes := U2B (Unsigned (p.Indx2Units (indx)));
    if numBytes <= UInt32 (p.HiUnit - p.LoUnit) then
      retVal := p.LoUnit;
      p.LoUnit := p.LoUnit + numBytes;
      node_ref := retVal;
      return;
    end if;
    AllocUnitsRare (p, indx, node_ref);
  end AllocUnits;

  procedure ShrinkUnits (
    p            : in out CPpmd7;
    oldPtr       :        Big_mem_index;
    oldNU, newNU :        Unsigned;
    newPtr       : out    Big_mem_index)
  is
    i0 : Unsigned := U2I (p, oldNU);
    i1 : Unsigned := U2I (p, newNU);
    ptr : Big_mem_index;
  begin
    if i0 = i1 then
      newPtr := oldPtr;
      return;
    end if;
    if p.FreeList (i1) /= 0 then
      RemoveNode (p, i1, ptr);
      --  MyMem12Cpy(ptr, oldPtr, newNU);  !!!
      p.Base (ptr .. ptr + Big_mem_index (newNU) - 1) :=
        p.Base (oldPtr .. oldPtr + Big_mem_index (newNU) - 1);
      InsertNode (p, oldPtr, i0);
      newPtr := ptr;
      return;
    end if;
    SplitBlock (p, oldPtr, i0, i1);
    newPtr := oldPtr;
    return;
  end ShrinkUnits;

  --  !! Does this successor stuff need to be split into 16-bit variables ?...

  function SUCCESSOR (p : CPpmd_State) return CPpmd_Void_Ref is
  begin
    return UInt32 (p.SuccessorLow) or Shift_Left (UInt32 (p.SuccessorHigh), 16);
  end SUCCESSOR;

  procedure SetSuccessor (p : in out CPpmd_State; v : CPpmd_Void_Ref) is
  begin
    p.SuccessorLow  := UInt16 (UInt32 (v) and 16#FFFF#);
    p.SuccessorHigh := UInt16 (Shift_Right (UInt32 (v), 16) and 16#FFFF#);
  end SetSuccessor;

  function Convert is new Ada.Unchecked_Conversion (Byte_access, CTX_PTR);
  function Convert is new Ada.Unchecked_Conversion (Byte_access, CPpmd_State_access);

  procedure RestartModel (p : in out CPpmd7) is
    val : UInt16;
  begin
    p.FreeList := (others => 0);
    p.Text := p.AlignOffset;
    p.HiUnit := p.Text + p.Size;
    p.UnitsStart := p.HiUnit - p.Size / 8 / UNIT_SIZE * 7 * UNIT_SIZE;
    p.LoUnit := p.UnitsStart;
    p.GlueCount := 0;

    p.OrderFall := p.MaxOrder;
    p.InitRL := -Int32 (Unsigned'Min (p.MaxOrder, 12)) - 1;
    p.RunLength := p.InitRL;
    p.PrevSuccess := 0;

    p.HiUnit := p.HiUnit - UNIT_SIZE;
    p.MaxContext := Convert (p.Base (p.HiUnit)'Access); --  /* AllocContext(p); */
    p.MinContext := p.MaxContext;
    p.MinContext.Suffix := 0;
    p.MinContext.NumStats := 256;
    p.MinContext.SummFreq := 256 + 1;
    p.FoundState := p.LoUnit;  --  /* AllocUnits(p, PPMD_NUM_INDEXES - 1); */
    p.LoUnit := p.LoUnit + U2B (256 / 2);
    p.MinContext.Stats := p.FoundState;
    for i in 0 .. 255 loop
      declare
        s : CPpmd_State_access renames
              Convert (p.Base (p.FoundState + CPpmd_State_Byte_Size * CPpmd_State_Ref (i))'Access);
      begin
        s.Symbol := Byte (i);
        s.Freq   := 1;
        SetSuccessor (s.all, 0);
      end;
    end loop;

    for i in 0 .. 127 loop
      for k in 0 .. 7 loop
        val := PPMD_BIN_SCALE - kInitBinEsc (k) / UInt16 (i + 2);
        for m in 0 .. 7 loop
          p.BinSumm (i, k + 8 * m) := val;
        end loop;
      end loop;
    end loop;

    for i in 0 .. 24 loop
      for k in 0 .. 15 loop
        declare
          s : CPpmd_See renames p.See (i, k);
        begin
          s.Shift := PPMD_PERIOD_BITS - 4;
          s.Summ  := Shift_Left (UInt16 (5 * i + 10), Natural (s.Shift));
          s.Count := 4;
        end;
      end loop;
    end loop;
  end RestartModel;

  procedure Ppmd7_Init (p : in out CPpmd7; maxOrder : Unsigned) is
  begin
    p.MaxOrder := maxOrder;
    RestartModel (p);
    p.DummySee.Shift := PPMD_PERIOD_BITS;
    p.DummySee.Summ  := 0;    --  unused
    p.DummySee.Count := 64;   --  unused
  end Ppmd7_Init;

  function CTX (p : CPpmd7; ref: CPpmd7_Context_Ref) return CTX_PTR is
  begin
    return Convert (p.Base (ref)'Access);
  end CTX;

  function SUFFIX (p : CPpmd7; c: CPpmd7_Context) return CTX_PTR is
  begin
    return Convert (p.Base (c.Suffix)'Access);
  end SUFFIX;

  --  Turns an Address into an index in the big memory array (p.Base.all).

  function REF (p : CPpmd7; addr: System.Address) return Big_mem_index is
    use System, System.Storage_Elements;
    base_address : Address;
    diff : Storage_Offset;
  begin
    base_address := p.Base (0)'Address;
    diff := addr - base_address;
    --  Absent a bug, diff fits in 32 bits *and* in the index range of p.Base.all ...
    return Big_mem_index (diff);
  end REF;

  --  Turns a pointer into an index in the big memory array (p.Base.all).

  function REF (p : CPpmd7; c: CTX_PTR) return Big_mem_index is
  begin
    return REF (p, c.all'Address);
  end REF;

  procedure CreateSuccessors(p : in out CPpmd7; skip : Boolean; c : out CPpmd7_Context_access) is
    upState      : CPpmd_State;
    found_state  : CPpmd_State_access := Convert (p.Base (p.FoundState)'Access);
    upBranch     : CPpmd_Byte_Ref := SUCCESSOR(found_state.all);
    PPMD7_MAX_ORDER : constant := 64;
    ps           : array (0 .. PPMD7_MAX_ORDER - 1) of CPpmd_State_access;
    numPs        : Natural := 0;
    successor_lc : CPpmd_Void_Ref;  --  !!  _lc = lower case
    s            : CPpmd_State_access;
    cf, s0       : UInt32;
    c1           : CPpmd7_Context_access;
  begin    
    c := p.MinContext;
    if not skip then
      numPs := numPs + 1;
      ps (numPs) := found_state;
    end if;
    --
    while c.Suffix /= 0 loop
      c := SUFFIX(p, c.all);
      if c.NumStats /= 1 then
        s := STATS(c);
        while s.Symbol = found_state.Symbol loop
          null;  --  !!! s++ pointer increment  !!!
        end loop;
      else
        s := ONE_STATE(c);
      end if;
      successor_lc := SUCCESSOR(s.all);
      if successor_lc /= upBranch then
        c := CTX(p, successor_lc);
        if numPs = 0 then
          return;
        end if;
        exit;
      end if;
      numPs := numPs + 1;
      ps (numPs) := s;
    end loop;
    
    --  !!!  upState.Symbol := *(const Byte *)Ppmd7_GetPtr(p, upBranch);
    SetSuccessor(upState, upBranch + 1);
    
    if c.NumStats = 1 then
      upState.Freq := ONE_STATE(c).Freq;
    else
      s := STATS(c);
      while s.Symbol = upState.Symbol loop
        null;  --  !!! s++ pointer increment  !!!
      end loop;
      --  !!! not translated !!!
      --    cf = s->Freq - 1;
      --    s0 = c->SummFreq - c->NumStats - cf;
      --    upState.Freq = (Byte)(1 + ((2 * cf <= s0) ? (5 * cf > s0) : ((2 * cf + 3 * s0 - 1) / (2 * s0))));
    end if;
   
    loop
      --  /* Create Child: AllocContext(p); */
      
      --  !!! not translated !!!
      --
      --  if (p->HiUnit != p->LoUnit)
      --    c1 = (CTX_PTR)(p->HiUnit -= UNIT_SIZE);
      --  else if (p->FreeList[0] != 0)
      --    c1 = (CTX_PTR)RemoveNode(p, 0);
      --  else
      --  {
      --    c1 = (CTX_PTR)AllocUnitsRare(p, 0);
      --    if (!c1)
      --      c := null;
      --  }
      --  c1->NumStats = 1;
      --  *ONE_STATE(c1) = upState;
      c1.Suffix := REF(p, c);
      numPs := numPs - 1;
      SetSuccessor(ps(numPs).all, REF(p, c1));
      c := c1;
      exit when numPs = 0;
    end loop;
    
  end CreateSuccessors;

  procedure SwapStates(t1, t2 : in out CPpmd_State) is
    tmp : CPpmd_State;
  begin
    tmp := t1;
    t1  := t2;
    t2  := tmp;
  end SwapStates;

  procedure UpdateModel(p : in out CPpmd7) is
    successor_lc : CPpmd_Void_Ref;  --  !!  _lc = lower case
    found_state  : CPpmd_State_access := Convert (p.Base (p.FoundState)'Access);
    fSuccessor   : CPpmd_Void_Ref := SUCCESSOR(found_state.all);
    c            : CTX_PTR;
    s0, ns, ns1  : unsigned;
    s, s_old     : CPpmd_State_access;
    cs           : CPpmd7_Context_access;
    cf, sf       : UInt32;
    i, oldNU     : unsigned;
    s_node_ref   : Big_mem_index;
  begin  
    if found_state.Freq < MAX_FREQ / 4 and then p.MinContext.Suffix /= 0 then
      c := SUFFIX(p, p.MinContext.all);
      --
      if c.NumStats = 1 then
        s := ONE_STATE(c);
        if s.Freq < 32 then
          s.Freq := s.Freq + 1;
        end if;
      else
        s := STATS(c);
        if s.Symbol /= found_state.Symbol then
          loop
            s_old := s;
            null;  --  !!! s++ pointer increment  !!!
            exit when s.Symbol = found_state.Symbol;
          end loop;
          if s.Freq >= s_old.Freq then
            SwapStates(s.all, s_old.all);
            s := s_old;
          end if;
        end if;
        if s.Freq < MAX_FREQ - 9 then
          s.Freq := s.Freq + 2;          --  Increase frequency in state s
          c.SummFreq := c.SummFreq + 2;  --  Increase frequency in context c
        end if;
      end if;
    end if;

    if p.OrderFall = 0 then
      CreateSuccessors(p, True, p.MaxContext);
      p.MinContext := p.MaxContext;
      if p.MinContext = null then
        RestartModel(p);
        return;
      end if;
      SetSuccessor(found_state.all, REF(p, p.MinContext));
      return;
    end if;
    
    p.Text := p.Text + 1;
    p.Base (p.Text) := found_state.Symbol;
    successor_lc := p.Text;
    if p.Text >= p.UnitsStart then  --  The text area will overlap the Units area
      RestartModel(p);
      return;
    end if;
    
    if fSuccessor /= 0 then
      if fSuccessor <= successor_lc then
        CreateSuccessors(p, False, cs);
        if cs = null then
          RestartModel(p);
          return;
        end if;
        fSuccessor := REF(p, cs);
      end if;
      p.OrderFall := p.OrderFall - 1;
      if p.OrderFall = 0 then
        successor_lc := fSuccessor;
        if p.MaxContext /= p.MinContext then
          p.Text := p.Text - 1;
        end if;
      end if;
    else
      SetSuccessor(found_state.all, successor_lc);
      fSuccessor := REF(p, p.MinContext);
    end if;
    
    ns := unsigned (p.MinContext.NumStats);
    s0 := unsigned (p.MinContext.SummFreq) - ns - unsigned (found_state.Freq - 1);

    c := p.MaxContext;
    while c /= p.MinContext loop
      ns1 := unsigned (c.NumStats);
      if ns1 /= 1 then
        if (ns1 and 1) = 0 then
          --  /* Expand for one UNIT */
          oldNU := ns1 / 2;
          i := U2I(p, oldNU);
          if i /= U2I(p, oldNU + 1) then
            --  !!! not translated !!!
            null;
            --  void *ptr = AllocUnits(p, i + 1);
            --  void *oldPtr;
            --  if (!ptr)
            --  {
            --    RestartModel(p);
            --    return;
            --  }
            --  oldPtr = STATS(c);
            --  MyMem12Cpy(ptr, oldPtr, oldNU);
            --  InsertNode(p, oldPtr, i);
            --  c.Stats := STATS_REF(ptr);
          end if;
        end if;
        c.SummFreq := UInt16(c.SummFreq + Boolean'Pos(2 * ns1 < ns) + 2 * (Boolean'Pos(4 * ns1 <= ns) and Boolean'Pos(Unsigned (c.SummFreq) <= 8 * ns1)));
      else
        AllocUnits(p, 0, s_node_ref);
        if s_node_ref = 0 then
          RestartModel(p);
          return;
        end if;
        s := Convert (p.Base (s_node_ref)'Access);
        s.all := ONE_STATE(c).all;
        c.Stats := REF(p, s.all'Address);
        if s.Freq < MAX_FREQ / 4 - 1 then
          s.Freq := 2 * s.Freq;
        else
          s.Freq := MAX_FREQ - 4;
        end if;
        c.SummFreq := UInt16(Unsigned(s.Freq) + p.InitEsc + Boolean'Pos(ns > 3));
      end if;
      cf := 2 * UInt32(found_state.Freq) * UInt32(c.SummFreq + 6);
      sf := UInt32 (s0) + UInt32(c.SummFreq);
      if cf < 6 * sf then
        cf := 1 + Boolean'Pos(cf > sf) + Boolean'Pos(cf >= 4 * sf);
        c.SummFreq := c.SummFreq + 3;
      else
        cf := 4 + Boolean'Pos(cf >= 9 * sf) + Boolean'Pos(cf >= 12 * sf) + Boolean'Pos(cf >= 15 * sf);
        c.SummFreq := UInt16(Uint32(c.SummFreq) + cf);
      end if;
      begin  --  NB: This block is also in the C code
        s := STATS(c) + ns1;
        SetSuccessor(s.all, successor_lc);
        s.Symbol := found_state.Symbol;
        s.Freq := Byte(cf);
        c.NumStats := UInt16(ns1 + 1);
      end;
      c := SUFFIX(p, c.all);
    end loop;
    p.MinContext := CTX(p, fSuccessor);
    p.MaxContext := p.MinContext;
  end UpdateModel;
  
end PPMd.Model;
