--  Experimental/recreative implementations of the Burrows-Wheeler
--  Transform (BWT), a block-sorting pre-processing for improving
--  data compression (this step is called pre-compression).
--
--  https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
--
package BWT is

  type Encoding_Method is
    (matrix_sorting,  --  Build a matrix with rotated strings and sort them (super-slow and memory-hungry!).
     index_sorting,   --  Sort the indices of the rotated strings (the matrix is not built in real).
     suffix_array);   --  Use a Suffix Array.

  procedure Encode (message : in out String; index : out Positive; method : Encoding_Method);

  procedure Decode (message : in out String; index : in Positive);

end BWT;
