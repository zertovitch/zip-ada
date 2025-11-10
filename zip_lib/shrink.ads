--  Shrink
----------
--
--  Shrink is a variant of the LZW (Ziv-Lempel-Welch) algorithm.
--
------------------------------------------
--  Excerpt from PKWare Zip's Appnote.txt:
--
--    5.1.1 Shrinking is a Dynamic Ziv-Lempel-Welch compression algorithm
--    with partial clearing.  The initial code size is 9 bits, and the
--    maximum code size is 13 bits.  Shrinking differs from conventional
--    Dynamic Ziv-Lempel-Welch implementations in several respects:
--
--    5.1.2 The code size is controlled by the compressor, and is
--    not automatically increased when codes larger than the current
--    code size are created (but not necessarily used).  When
--    the decompressor encounters the code sequence 256
--    (decimal) followed by 1, it SHOULD increase the code size
--    read from the input stream to the next bit size.  No
--    blocking of the codes is performed, so the next code at
--    the increased size SHOULD be read from the input stream
--    immediately after where the previous code at the smaller
--    bit size was read.  Again, the decompressor SHOULD NOT
--    increase the code size used until the sequence 256,1 is
--    encountered.
--
--    5.1.3 When the table becomes full, total clearing is not
--    performed.  Rather, when the compressor emits the code
--    sequence 256,2 (decimal), the decompressor SHOULD clear
--    all leaf nodes from the Ziv-Lempel tree, and continue to
--    use the current code size.  The nodes that are cleared
--    from the Ziv-Lempel tree are then re-used, with the lowest
--    code value re-used first, and the highest code value
--    re-used last.  The compressor can emit the sequence 256,2
--    at any time.

package Shrink is

  Special_Code : constant := 256;
  First_Entry  : constant := 257;

  Code_for_increasing_code_size : constant := 1;
  Code_for_clearing_table       : constant := 2;

end Shrink;
