with Ada.Containers.Vectors;

generic

  type Index is range <>;
  type Alphabet is (<>);  --  Any discrete type (usually: Byte, Unsigned_8).
  type Buffer_Type is array (Index range <>) of Alphabet;

  discrepancy_threshold : Float;  --  Discrepancy detection threshold (e.g.: 1.9).
  index_threshold       : Index;  --  Do segmentation only above a certain distance (e.g.: 20_000).
  window_size           : Index;  --  Sliding window size (e.g.: 10_000).

package Data_Segmentation is

  subtype Positive_Index is Index range 1 .. Index'Last;
  package Index_Vectors is new Ada.Containers.Vectors (Positive, Positive_Index);
  subtype Segmentation is Index_Vectors.Vector;

  procedure Segment_by_Entropy (buffer : in Buffer_Type; seg : out Segmentation);

end Data_Segmentation;
