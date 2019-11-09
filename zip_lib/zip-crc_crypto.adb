--  Legal licensing note:

--  Copyright (c) 1999 .. 2019 Gautier de Montmollin
--  SWITZERLAND

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found on the site
--  http://www.opensource.org/licenses/mit-license.php

package body Zip.CRC_Crypto is

  CRC32_Table : array (Unsigned_32'(0) .. 255) of Unsigned_32;

  procedure Prepare_table is
    --  CRC-32 algorithm, ISO-3309
    Seed : constant := 16#EDB88320#;
    l : Unsigned_32;
  begin
    for i in CRC32_Table'Range loop
      l := i;
      for bit in 0 .. 7 loop
        if (l and 1) = 0 then
          l := Shift_Right (l, 1);
        else
          l := Shift_Right (l, 1) xor Seed;
        end if;
      end loop;
      CRC32_Table (i) := l;
    end loop;
  end Prepare_table;

  procedure Update (CRC : in out Unsigned_32; InBuf : Zip.Byte_Buffer) is
    local_CRC : Unsigned_32;
  begin
    local_CRC := CRC;
    for i in InBuf'Range loop
      local_CRC :=
        CRC32_Table (16#FF# and (local_CRC xor Unsigned_32 (InBuf (i))))
        xor
        Shift_Right (local_CRC, 8);
    end loop;
    CRC := local_CRC;
  end Update;

  table_empty : Boolean := True;

  procedure Init (CRC : out Unsigned_32) is
  begin
    if table_empty then
      Prepare_table;
      table_empty := False;
    end if;
    CRC := 16#FFFF_FFFF#;
  end Init;

  function Final (CRC : Unsigned_32) return Unsigned_32 is
  begin
    return not CRC;
  end Final;

  --

  procedure Set_mode (obj : in out Crypto_pack; new_mode : Crypto_Mode) is
  begin
    obj.current_mode := new_mode;
  end Set_mode;

  function Get_mode (obj : Crypto_pack) return Crypto_Mode is
  begin
    return obj.current_mode;
  end Get_mode;

  procedure Update_keys (obj : in out Crypto_pack; by : Zip.Byte) is
  begin
    Update (obj.keys (0), (0 => by));
    obj.keys (1) := obj.keys (1) + (obj.keys (0) and 16#000000ff#);
    obj.keys (1) := obj.keys (1) * 134775813 + 1;
    Update (
      obj.keys (2),
      (0 => Zip.Byte (Shift_Right (obj.keys (1), 24)))
    );
  end Update_keys;

  --  Crypto_code: Pseudo-random byte to be XOR'ed with.
  function Crypto_code (obj : Crypto_pack) return Zip.Byte is
  pragma Inline (Crypto_code);
    temp : Unsigned_16;
  begin
    temp := Unsigned_16 (obj.keys (2) and 16#ffff#) or 2;
    return Zip.Byte (Shift_Right (temp * (temp xor 1), 8));
  end Crypto_code;

  procedure Init_keys (obj : in out Crypto_pack; password : String) is
  begin
    obj.keys := (16#12345678#, 16#23456789#, 16#34567890#);
    for i in password'Range loop
      Update_keys (obj, Character'Pos (password (i)));
    end loop;
  end Init_keys;

  procedure Encode (obj : in out Crypto_pack; buf : in out Zip.Byte_Buffer) is
    bc : Zip.Byte;
  begin
    if obj.current_mode = encrypted then
      for i in buf'Range loop
        bc := buf (i);
        buf (i) := bc xor Crypto_code (obj);
        Update_keys (obj, bc);  --  Keys are updated with the unencrypted byte
      end loop;
    end if;
  end Encode;

  procedure Decode (obj : in out Crypto_pack; b : in out Unsigned_8) is
  begin
    if obj.current_mode = encrypted then
      b := b xor Crypto_code (obj);
      Update_keys (obj, b);     --  Keys are updated with the unencrypted byte
    end if;
  end Decode;

end Zip.CRC_Crypto;
