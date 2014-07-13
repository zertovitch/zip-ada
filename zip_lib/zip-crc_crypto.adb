package body Zip.CRC_Crypto is

  CRC32_Table : array( Unsigned_32'(0)..255 ) of Unsigned_32;

  procedure Prepare_table is
    -- CRC-32 algorithm, ISO-3309
    Seed: constant:= 16#EDB88320#;
    l: Unsigned_32;
  begin
    for i in CRC32_Table'Range loop
      l:= i;
      for bit in 0..7 loop
        if (l and 1) = 0 then
          l:= Shift_Right(l,1);
        else
          l:= Shift_Right(l,1) xor Seed;
        end if;
      end loop;
      CRC32_Table(i):= l;
    end loop;
  end Prepare_table;

  procedure Update( CRC: in out Unsigned_32; InBuf: Zip.Byte_Buffer ) is
    local_CRC: Unsigned_32;
  begin
    local_CRC:= CRC ;
    for i in InBuf'Range loop
      local_CRC :=
        CRC32_Table( 16#FF# and ( local_CRC xor Unsigned_32( InBuf(i) ) ) )
        xor
        Shift_Right( local_CRC , 8 );
    end loop;
    CRC:= local_CRC;
  end Update;

  table_empty: Boolean:= True;

  procedure Init( CRC: out Unsigned_32 ) is
  begin
    if table_empty then
      Prepare_table;
      table_empty:= False;
    end if;
    CRC:= 16#FFFF_FFFF#;
  end Init;

  function Final( CRC: Unsigned_32 ) return Unsigned_32 is
  begin
    return not CRC;
  end Final;

  --

  package body Crypto is -- 27-Jun-2001: Algorithm in Appnote.txt

    type Decrypt_keys is array( 0..2 ) of Unsigned_32;
    keys     : Decrypt_keys;
    current_mode : Mode;

    procedure Set_mode ( new_mode: Mode ) is
    begin
      current_mode:= new_mode;
    end Set_mode;

    function Get_mode return Mode is
    begin
      return current_mode;
    end Get_mode;

    procedure Update_keys( by: Zip.Byte ) is
    begin
      Update( keys(0), (0 => by) );
      keys(1) := keys(1) + (keys(0) and 16#000000ff#);
      keys(1) := keys(1) * 134775813 + 1;
      Update(
        keys(2),
        (0 => Zip.Byte(Shift_Right( keys(1), 24 )))
      );
    end Update_keys;

    function Crypto_code return Zip.Byte is -- Pseudo-random byte to be XOR'ed
      temp: Unsigned_16;
    begin
      temp:= Unsigned_16(keys(2) and 16#ffff#) or 2;
      return Zip.Byte(Shift_Right(temp * (temp xor 1), 8));
    end Crypto_code;

    procedure Init_keys(pwd: String) is
    begin
      keys:= ( 16#12345678#, 16#23456789#, 16#34567890# );
      for i in pwd'Range loop
        Update_keys( Character'Pos(pwd(i)) );
      end loop;
    end Init_keys;

    procedure Decode( b: in out Zip.Byte ) is
    begin
      if current_mode = encrypted then
        b:= b xor Crypto_code;
        Update_keys(b); -- Keys are updated with the unencrypted byte
      end if;
    end Decode;

  end Crypto;

end Zip.CRC_Crypto;
