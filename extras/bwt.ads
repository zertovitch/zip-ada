--  Burrows-Wheeler transform: block-sorting preprocessing
--  for improving data compression (precompression).
--
--  https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
--
package BWT is

  procedure Encode (message : in out String; index : out Positive);

  procedure Decode (message : in out String; index : in Positive);

end BWT;
