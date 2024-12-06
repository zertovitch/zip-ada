--  Package to construct and manipulate suffix arrays

--  Authors: Rob Irving, Lorna Love
--  Final version: 24 January 2001

--  Found @ https://www.dcs.gla.ac.uk/research/algorithms/sbst/software/Ada95/sa/src/
--  in December 2024.
--  Read also:
--    https://www.dcs.gla.ac.uk/research/algorithms/sbst/software/Ada95/sa/READ_ME.htm 

--  Minor modifications (GdM 2024):
--    -  Abstracted the string type through generics.
--    -  Added function Suffix for querying the suffix array.
--    -  Turned Fully_Search_Sa into a function (rids of Integer_List_Package)
--    -  Addressed a warning and an uninitialized variable issue.

generic

  type Symbol is (<>);
  --
  --  Symbol is any discrete type
  --  E.g., for representing Character or Unsigned_8 with a terminator symbol: range -1 .. 255.
  --  In that case, Unused_Symbol (defined below) is -1 can be used as terminator.
  --  Unused_Symbol is used as unused symbol for Build_Gen_Sbst.
  --  You can use directly Unsigned_8, Character, Wide_Character, ... instead, without
  --  using the terminator symbol.

  type Count_Type is range <>;  --  Index type: Integer, Integer_32, Integer_64, ...

  type Symbol_Array is array (Count_Type range <>) of Symbol;  --  For Buffer, Wide_String, ...

package Suffix_Arrays is

  Unused_Symbol : constant Symbol := Symbol'First;

  type Sa_Element is private;
  type Sa_Type is array (Count_Type range <>) of Sa_Element;

  --  - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  procedure Output_Sa (Sa : in Sa_Type; F_Name : in String);
  --  Outputs the suffix array Sa to the file F_Name.

  function Suffix (Sa : in Sa_Type; Index : Count_Type) return Count_Type with Inline;
  --  Query the suffix array.

  --  - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  procedure Build_Sa (S : in Symbol_Array; A : out Sa_Type);
  --  Builds the suffix array A for the Element_Array S.

  --  - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  procedure Build_Gen_Sa (S1, S2 : in Symbol_Array; A : out Sa_Type);
  --  Builds a 'generalised' suffix array, A, for Element_Array's S1 and S2.

  --  - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  procedure Search_Sa
   (A : in Sa_Type; S, X : in Symbol_Array; Found : out Boolean; Pos : out Count_Type);
  --  Searches the suffix array A, of Element_Array S, for a target Element_Array X;
  --  Returns Found = False if X is not a substring of S, otherwise
  --  returns Found = True, and Pos such that X occurs at S (Pos).

  --  - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  type Count_Array is array (Count_Type range <>) of Count_Type;

  function Fully_Search_Sa (A : in Sa_Type; S, X : in Symbol_Array)
    return Count_Array;
  --  Searches the suffix array A, of Element_Array S, for a target Element_Array X; returns
  --  in Pos_List a list of all the positions of S where X occurs as a substring.

  --  - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  function Num_Occurrences (A : Sa_Type; S, X : Symbol_Array) return Count_Type;
  --  Searches the suffix array A, of Element_Array S, for a target Element_Array X;
  --  returns the number of occurrences of X in S
  --  - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  procedure Traverse_For_Lrs (A : in Sa_Type; Pos1, Pos2, Len : out Count_Type);
  --  Traverses the suffix array A (of a Element_Array S), and returns Pos1, Pos2 and
  --  Len, such that S (Pos1 .. Pos1 + Len - 1) = S (Pos2 .. Pos2 + Len - 1)
  --  is an occurrence of a longest repeated substring of S.

  --  - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  procedure Traverse_For_Lcs
   (A : in Sa_Type; Len_S1 : in Count_Type; Pos1, Pos2, Len : out Count_Type);
  --  Traverses the generalised suffix array A (of strings S1 and S2), and returns
  --  Pos1, Pos2 and Len such that
  --  S1 (Pos1 .. Pos1 + Len - 1) = S2 (Pos2 .. Pos2 + Len - 1)
  --  is an occurrence of a longest common substring of S1 and S2.
  --  Len_S1 is the length of Element_Array S1 (essential information for the algorithm)

private

  --  This is the version of a suffix array in which each array
  --  position has a single associated lcp and direction value

  type Dirn_Type is (Left, Right);

  type Sa_Element is record
    Suffix : Count_Type;
    Lcp    : Count_Type;
    Dir    : Dirn_Type;
  end record;

end Suffix_Arrays;
