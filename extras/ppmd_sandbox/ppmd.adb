with Ada.Unchecked_Deallocation;

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

  type CPpmd7_Node_Ref is access CPpmd7_Node;

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

end PPMd;
