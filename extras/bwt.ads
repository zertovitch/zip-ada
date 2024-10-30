--  Burrows-Wheeler transform: block-sorting pre-processing
--  for improving data compression (this step is called pre-compression).
--
--  https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
--
package BWT is

  procedure Encode (message : in out String; index : out Positive; smart : Boolean := False);

  procedure Decode (message : in out String; index : in Positive);

end BWT;
