-- Zip.CRC_Crypto deals with pseudo-random generators for data integrity check and encryption
--
-- CRC: Cyclic redundancy check to verify archived data integrity

package Zip.CRC_Crypto is

  use Interfaces;

  procedure Init( CRC: out Unsigned_32 );

  function  Final( CRC: Unsigned_32 ) return Unsigned_32;

  procedure Update( CRC: in out Unsigned_32; InBuf: Zip.Byte_Buffer );
  pragma Inline( Update );

  type Crypto_pack is private;
  --
  type Crypto_Mode is (clear, encrypted);
  procedure Set_mode(obj: in out Crypto_pack; new_mode: Crypto_Mode);
  function Get_mode(obj: Crypto_pack) return Crypto_Mode;
  --
  procedure Init_keys(obj: in out Crypto_pack; password: String);
  --
  procedure Encode(obj: in out Crypto_pack; buf: in out Zip.Byte_Buffer);
    pragma Inline(Encode);
  --
  procedure Decode(obj: in out Crypto_pack; b: in out Unsigned_8);
    pragma Inline(Decode);

private
  type Decrypt_keys is array( 0..2 ) of Unsigned_32;
  type Crypto_pack is record
    keys         : Decrypt_keys;
    current_mode : Crypto_Mode;
  end record;

end Zip.CRC_Crypto;
