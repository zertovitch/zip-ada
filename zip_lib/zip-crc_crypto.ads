--  Zip.CRC_Crypto deals with hash-like functions
--  for data integrity check and encryption
-------------------------------------------------

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

package Zip.CRC_Crypto is

  use Interfaces;

  -------------------------------------------------------------
  --  CRC: Cyclic Redundancy Check to verify data integrity  --
  -------------------------------------------------------------

  procedure Init (CRC : out Unsigned_32);

  procedure Update (CRC : in out Unsigned_32; InBuf : Zip.Byte_Buffer);
  pragma Inline (Update);

  function  Final (CRC : Unsigned_32) return Unsigned_32;
  pragma Inline (Final);

  ------------------
  --  Encryption  --
  ------------------

  type Crypto_pack is private;
  --
  type Crypto_Mode is (clear, encrypted);
  procedure Set_mode (obj : in out Crypto_pack; new_mode : Crypto_Mode);
  function Get_mode (obj : Crypto_pack) return Crypto_Mode;
  --
  procedure Init_keys (obj : in out Crypto_pack; password : String);
  --
  procedure Encode (obj : in out Crypto_pack; buf : in out Zip.Byte_Buffer);
  pragma Inline (Encode);
  --
  procedure Decode (obj : in out Crypto_pack; b : in out Unsigned_8);
  pragma Inline (Decode);

private
  type Decrypt_keys is array (0 .. 2) of Unsigned_32;
  type Crypto_pack is record
    keys         : Decrypt_keys;
    current_mode : Crypto_Mode;
  end record;

end Zip.CRC_Crypto;
