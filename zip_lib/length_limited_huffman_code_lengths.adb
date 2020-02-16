--  Legal licensing note:

--  Copyright (c) 2016 .. 2019 Gautier de Montmollin (maintainer of the Ada version)
--  SWITZERLAND
--
--  The copyright holder is only the maintainer of the Ada version;
--  authors of the C code and those of the algorithm are cited below.

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

--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

--  Author: lode.vandevenne [*] gmail [*] com (Lode Vandevenne)
--  Author: jyrki.alakuijala [*] gmail [*] com (Jyrki Alakuijala)

--  Bounded package merge algorithm, based on the paper
--    "A Fast and Space-Economical Algorithm for Length-Limited Coding
--    Jyrki Katajainen, Alistair Moffat, Andrew Turpin".

--  Translated by G. de Montmollin to Ada from katajainen.c (Zopfli project), 7-Feb-2016
--
--  Main technical differences to katajainen.c:
--    - pointers are not used, array indices instead
--    - all structures are allocated on stack
--    - sub-programs are nested, then unneeded parameters are removed

procedure Length_limited_Huffman_code_lengths (
  frequencies : in  Count_Array;
  bit_lengths : out Length_Array
)
is
  subtype Index_Type is Count_Type;

  null_index : constant Index_Type := Index_Type'Last;

  --  Nodes forming chains.
  type Node is record
    weight : Count_Type;
    count  : Count_Type;                --  Number of leaves before this chain.
    tail   : Index_Type := null_index;  --  Previous node(s) of this chain, or null_index if none.
    in_use : Boolean    := False;       --  Tracking for garbage collection.
  end record;

  type Leaf_Node is record
    weight : Count_Type;
    symbol : Alphabet;
  end record;

  --  Memory pool for nodes.
  pool : array (0 .. Index_Type (2 * max_bits * (max_bits + 1) - 1)) of Node;
  pool_next : Index_Type := pool'First;

  type Index_pair is array (Index_Type'(0) .. 1) of Index_Type;
  lists : array (0 .. Index_Type (max_bits - 1)) of Index_pair;

  type Leaf_array is array (Index_Type range <>) of Leaf_Node;
  leaves : Leaf_array (0 .. frequencies'Length - 1);

  num_symbols : Count_Type := 0;  --  Amount of symbols with frequency > 0.
  num_Boundary_PM_runs : Count_Type;

  too_many_symbols_for_length_limit : exception;
  zero_length_but_nonzero_frequency : exception;
  nonzero_length_but_zero_frequency : exception;
  length_exceeds_length_limit       : exception;
  buggy_sorting                     : exception;

  procedure Init_Node (weight, count : Count_Type; tail, node_idx : Index_Type) is
  begin
    pool (node_idx).weight := weight;
    pool (node_idx).count  := count;
    pool (node_idx).tail   := tail;
    pool (node_idx).in_use := True;
  end Init_Node;

  --  Finds a free location in the memory pool. Performs garbage collection if needed.
  --  If use_lists = True, used to mark in-use nodes during garbage collection.

  function Get_Free_Node (use_lists : Boolean) return Index_Type is
    node_idx : Index_Type;
  begin
    loop
      if pool_next > pool'Last then
        --  Garbage collection.
        for i in pool'Range loop
          pool (i).in_use := False;
        end loop;
        if use_lists then
          for i in 0 .. Index_Type (max_bits * 2 - 1) loop
            node_idx := lists (i / 2)(i mod 2);
            while node_idx /= null_index loop
              pool (node_idx).in_use := True;
              node_idx := pool (node_idx).tail;
            end loop;
          end loop;
        end if;
        pool_next := pool'First;
      end if;
      exit when not pool (pool_next).in_use;  -- Found one.
      pool_next := pool_next + 1;
    end loop;
    pool_next := pool_next + 1;
    return pool_next - 1;
  end Get_Free_Node;

  --  Performs a Boundary Package-Merge step. Puts a new chain in the given list. The
  --  new chain is, depending on the weights, a leaf or a combination of two chains
  --  from the previous list.
  --  index: The index of the list in which a new chain or leaf is required.
  --  final: Whether this is the last time this function is called. If it is then it
  --  is no more needed to recursively call self.

  procedure Boundary_PM (index : Index_Type; final : Boolean) is
    newchain  : Index_Type;
    oldchain  : Index_Type;
    lastcount : constant Count_Type := pool (lists (index)(1)).count;  --  Count of last chain of list.
    sum : Count_Type;
  begin
    if index = 0 and lastcount >= num_symbols then
      return;
    end if;
    newchain := Get_Free_Node (use_lists => True);
    oldchain := lists (index)(1);
    --  These are set up before the recursive calls below, so that there is a list
    --  pointing to the new node, to let the garbage collection know it's in use.
    lists (index) := (oldchain, newchain);

    if index = 0 then
      --  New leaf node in list 0.
      Init_Node (leaves (lastcount).weight, lastcount + 1, null_index, newchain);
    else
      sum := pool (lists (index - 1)(0)).weight + pool (lists (index - 1)(1)).weight;
      if lastcount < num_symbols and then sum > leaves (lastcount).weight then
        --  New leaf inserted in list, so count is incremented.
        Init_Node (leaves (lastcount).weight, lastcount + 1, pool (oldchain).tail, newchain);
      else
        Init_Node (sum, lastcount, lists (index - 1)(1), newchain);
        if not final then
          --  Two lookahead chains of previous list used up, create new ones.
          Boundary_PM (index - 1, False);
          Boundary_PM (index - 1, False);
        end if;
      end if;
    end if;
  end Boundary_PM;

  --  Initializes each list with as lookahead chains the two leaves with lowest weights.

  procedure Init_Lists is
    node0 : constant Index_Type := Get_Free_Node (use_lists => False);
    node1 : constant Index_Type := Get_Free_Node (use_lists => False);
  begin
    Init_Node (leaves (0).weight, 1, null_index, node0);
    Init_Node (leaves (1).weight, 2, null_index, node1);
    lists := (others => (node0, node1));
  end Init_Lists;

  --  Converts result of boundary package-merge to the bit_lengths. The result in the
  --  last chain of the last list contains the amount of active leaves in each list.
  --  chain: Chain to extract the bit length from (last chain from last list).

  procedure Extract_Bit_Lengths (chain : Index_Type) is
    node_idx : Index_Type := chain;
  begin
    while node_idx /= null_index loop
      for i in 0 .. pool (node_idx).count - 1 loop
        bit_lengths (leaves (i).symbol) := bit_lengths (leaves (i).symbol) + 1;
      end loop;
      node_idx := pool (node_idx).tail;
    end loop;
  end Extract_Bit_Lengths;

  function "<"(a, b : Leaf_Node) return Boolean is
  begin
    return a.weight < b.weight;
  end "<";

  procedure Quick_sort (a : in out Leaf_array) is
    n : constant Index_Type := a'Length;
    i, j : Index_Type;
    p, t : Leaf_Node;
  begin
    if n < 2 then
      return;
    end if;
    p := a (n / 2 + a'First);
    i := 0;
    j := n - 1;
    loop
      while a (i + a'First) < p loop
        i := i + 1;
      end loop;
      while p < a (j + a'First) loop
        j := j - 1;
      end loop;
      exit when i >= j;
      t := a (i + a'First);
      a (i + a'First) := a (j + a'First);
      a (j + a'First) := t;
      i := i + 1;
      j := j - 1;
    end loop;
    Quick_sort (a (a'First .. a'First + i - 1));
    Quick_sort (a (a'First + i .. a'Last));
  end Quick_sort;

  paranoid : constant Boolean := False;

begin
  bit_lengths := (others => 0);
  --  Count used symbols and place them in the leaves.
  for a in Alphabet loop
    if frequencies (a) > 0 then
      leaves (num_symbols) := (frequencies (a), a);
      num_symbols := num_symbols + 1;
    end if;
  end loop;
  --  Check special cases and error conditions.
  if num_symbols > 2 ** max_bits then
    raise too_many_symbols_for_length_limit;  --  Error, too few max_bits to represent symbols.
  end if;
  if num_symbols = 0 then
    return;  --  No symbols at all. OK.
  end if;
  if num_symbols = 1 then
    bit_lengths (leaves (0).symbol) := 1;
    return;  --  Only one symbol, give it bit length 1, not 0. OK.
  end if;
  --  Sort the leaves from lightest to heaviest.
  Quick_sort (leaves (0 .. num_symbols - 1));
  if paranoid then
    for i in 1 .. num_symbols - 1 loop
      if leaves (i) < leaves (i - 1) then
        raise buggy_sorting;
      end if;
    end loop;
  end if;
  Init_Lists;
  --  In the last list, 2 * num_symbols - 2 active chains need to be created. Two
  --  are already created in the initialization. Each Boundary_PM run creates one.
  num_Boundary_PM_runs := 2 * num_symbols - 4;
  for i in 1 .. num_Boundary_PM_runs loop
    Boundary_PM (Index_Type (max_bits - 1), i = num_Boundary_PM_runs);
  end loop;
  Extract_Bit_Lengths (lists (Index_Type (max_bits - 1))(1));
  if paranoid then
    --  Done; some checks before leaving. Not checked: completeness of Huffman codes.
    for a in Alphabet loop
      if frequencies (a) = 0 then
        if bit_lengths (a) > 0 then
          raise nonzero_length_but_zero_frequency;  --  Never happened so far
        end if;
      else
        if bit_lengths (a) = 0 then
          raise zero_length_but_nonzero_frequency;  --  Happened before null_index fix
        elsif bit_lengths (a) > max_bits then
          raise length_exceeds_length_limit;        --  Never happened so far
        end if;
      end if;
    end loop;
  end if;
end Length_limited_Huffman_code_lengths;
