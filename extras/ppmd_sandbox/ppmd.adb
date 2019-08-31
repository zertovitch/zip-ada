with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body PPMd is

  PPMD_INT_BITS    : constant := 7;
  PPMD_PERIOD_BITS : constant := 7;
  PPMD_BIN_SCALE   : constant := 2 ** (PPMD_INT_BITS + PPMD_PERIOD_BITS);

  procedure Ppmd_See_Update (p : in out CPpmd_See) is
  begin
    if p.Shift < PPMD_PERIOD_BITS then
      p.Count := p.Count - 1;  --  Likely to loop from 0 to 255, 254, 253, ...
      if p.Count = 0 then
        p.Summ  := Shift_Left (p.Summ, 1);
        p.Count := Shift_Left (3, Natural (p.Shift));  --  = 0 when p.Shift > 7
        p.Shift := p.Shift + 1;
      end if;
    end if;
  end Ppmd_See_Update;

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
      --  MyMem12Cpy(ptr, oldPtr, newNU);
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

  procedure RestartModel (p : in out CPpmd7) is
    i, k, m : Unsigned;
    function Convert is new Ada.Unchecked_Conversion (Byte_access, CTX_PTR);
    function Convert is new Ada.Unchecked_Conversion (Byte_access, CPpmd_State_access);
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
    p.FoundState := p.LoUnit; --  /* AllocUnits(p, PPMD_NUM_INDEXES - 1); */
    p.LoUnit := p.LoUnit + U2B (256 / 2);
    p.MinContext.Stats := p.FoundState;
    for i in 0 .. 255 loop
      declare
        --  !!! C question: does i index an array of bytes or CPpmd_State ??
        s : CPpmd_State renames Convert (p.FoundState (i)).all;
      begin
        s.Symbol := Byte (i);
        s.Freq := 1;
        SetSuccessor (s, 0);
      end;
    end loop;

    for i in 0 .. 127 loop
      for k in 0 .. 7 loop
        null; -- !!!
        --
        --  UInt16 *dest = p.BinSumm[i] + k;
        --  UInt16 val = (UInt16)(PPMD_BIN_SCALE - kInitBinEsc[k] / (i + 2));
        --  for (m = 0; m < 64; m += 8)
          --  dest[m] = val;
      end loop;
    end loop;

    for i in 0 .. 24 loop
      for k in 0 .. 15 loop
        declare
          s : CPpmd_See renames p.See (i)(k);
        begin
          s.Shift := PPMD_PERIOD_BITS - 4;
          s.Summ  := Shift_Left (UInt16 (5 * i + 10), Natural (s.Shift));
          s.Count := 4;
        end;
      end loop;
    end loop;
  end RestartModel;

end PPMd;
