--  This function is there to make cracking of encrypted
--  data more difficult. It returns a random string with
--  some repetitions. The idea is to put such a string at
--  the beginning of all uncompressed data to be encrypted
--  with the same encryption scheme and password to avoid
--  cracking with the known plaintext technique.
--  Minimum size recommended: 13 bytes of *compressed* data
--  (see PKCrack documentation).

function LZ_Scramble (length : Positive) return String;
