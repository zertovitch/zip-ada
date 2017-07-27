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

  procedure SplitBlock (p : in out CPpmd7; ptr_0 : Big_mem_index; oldIndx, newIndx : Unsigned) is
    i, nu, k : Unsigned;
    ptr : Big_mem_index;
  begin
    --  !! perhaps convert before subtraction :
    nu := Unsigned (p.Indx2Units (oldIndx) - p.Indx2Units (newIndx));
    ptr := ptr_0 + U2B (Unsigned (p.Indx2Units (newIndx)));
    i := Unsigned (p.Units2Indx (nu - 1));
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
      function Convert is new Ada.Unchecked_Conversion (Byte_access, CPpmd7_Node_access);
    begin
      return Convert (p.Base (ref)'Access);
    end NNODE;
    head  : CPpmd7_Node_Ref := p.AlignOffset + p.Size;
    n     : CPpmd7_Node_Ref := head;
    next  : CPpmd7_Node_Ref;
    node,
    node2 : CPpmd7_Node_access;
    nu16  : UInt16;
    nu32  : UInt32;
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
      while nuuu > 128 loop
        InsertNode (p, node, PPMD_NUM_INDEXES - 1);
        nuuu := nuuu - 128;
        node := node + 128;
      end loop;
      iu := Unsigned (p.Units2Indx (nuuu - 1));
      if p.Indx2Units (iu) /= nuuu then
        iu := iu - 1;
        k := p.Indx2Units (iu);
        InsertNode (p, node + k, nuuu - k - 1);
      end if;
      InsertNode (p, node, iu);
      n := next;
    end loop;
  end GlueFreeBlocks;

end PPMd;
