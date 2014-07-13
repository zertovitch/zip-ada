-- Zip.CRC_Crypto deals with pseudo-random generators for data integrity check and encryption
--
-- CRC: Cyclic redundancy check to verify archived data integrity

package Zip.CRC_Crypto is

  use Interfaces;

  procedure Init( CRC: out Unsigned_32 );

  function  Final( CRC: Unsigned_32 ) return Unsigned_32;

  procedure Update( CRC: in out Unsigned_32; InBuf: Zip.Byte_Buffer );
  pragma Inline( Update );

  generic
  package Crypto is
    --
    type Mode is (clear, encrypted);
    --
    procedure Set_mode( new_mode: Mode );
    function Get_mode return Mode;
    --
    procedure Init_keys(pwd: String);
    --
    procedure Decode( b: in out Unsigned_8 );
      pragma Inline(Decode);
  end Crypto;

end Zip.CRC_Crypto;
