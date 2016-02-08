procedure Length_limited_Huffman_code_lengths(
  frequencies :     Count_Array;
  bit_lengths : out Length_Array
)
is
  subtype Index_type is Count_Type;

  --  Nodes forming chains.
  type Node is record
    weight : Count_Type;
    count  : Count_Type;       --  Number of leaves before this chain.
    tail   : Index_type;       --  Previous node(s) of this chain, or 0 if none.
    in_use : Boolean:= False;  --  Tracking for garbage collection.
  end record;

  type Leaf_Node is record
    weight: Count_Type;
    symbol: Alphabet;
  end record;

  --  Memory pool for nodes.
  pool: array(0 .. Index_type(2 * max_bits * (max_bits + 1) - 1)) of Node;
  pool_next: Index_type:= pool'First;

  type Index_pair is array(Index_type'(0)..1) of Index_type;
  lists: array(0..Index_type(max_bits-1)) of Index_pair;

  type Leaf_array is array(Index_type range <>) of Leaf_Node;
  leaves: Leaf_array(0..frequencies'Length-1);

  num_symbols: Count_Type := 0;  --  Amount of symbols with frequency > 0.

  num_Boundary_PM_runs: Count_Type;

  procedure Init_Node(weight, count: Count_Type; tail, node: Index_type) is
  begin
    pool(node).weight := weight;
    pool(node).count  := count;
    pool(node).tail   := tail;
    pool(node).in_use := True;
  end Init_Node;

  --  Finds a free location in the memory pool. Performs garbage collection if needed.
  --  If use_lists = True, used to mark in-use nodes during garbage collection.

  function Get_Free_Node(use_lists: Boolean) return Index_type is
    node: Index_type;
  begin
    loop
      if pool_next >= pool'Last then
        --  Garbage collection.
        for i in pool'Range loop
          pool(i).in_use := False;
        end loop;
        if use_lists then
          for i in 0 .. Index_type(max_bits * 2 - 1) loop
            node:= lists(i / 2)(i mod 2);
            while node /= 0 loop
              pool(node).in_use := True;
              node := pool(node).tail;
            end loop;
          end loop;
        end if;
        pool_next:= pool'First;
      end if;
      exit when not pool(pool_next).in_use;  -- Found one.
      pool_next:= pool_next + 1;
    end loop;
    pool_next:= pool_next + 1;
    return pool_next - 1;
  end Get_Free_Node;

  --  Performs a Boundary Package-Merge step. Puts a new chain in the given list. The
  --  new chain is, depending on the weights, a leaf or a combination of two chains
  --  from the previous list.
  --  index: The index of the list in which a new chain or leaf is required.
  --  final: Whether this is the last time this function is called. If it is then it
  --  is no more needed to recursively call self.

  procedure Boundary_PM(index: Index_type; final: Boolean) is
    newchain: Index_type;
    oldchain: Index_type;
    lastcount: constant Count_type:= pool(lists(index)(1)).count;  --  Count of last chain of list.
    sum: Count_Type;
  begin
    if index = 0 and lastcount >= num_symbols then
      return;
    end if;
    newchain:= Get_Free_Node(use_lists => True);
    oldchain:= lists(index)(1);
    --  These are set up before the recursive calls below, so that there is a list
    --  pointing to the new node, to let the garbage collection know it's in use.
    lists(index) := (oldchain, newchain);

    if index = 0 then
      --  New leaf node in list 0.
      Init_Node(leaves(lastcount).weight, lastcount + 1, 0, newchain);
    else
      sum:= pool(lists(index - 1)(0)).weight + pool(lists(index - 1)(1)).weight;
      if lastcount < num_symbols and then sum > leaves(lastcount).weight then
        --  New leaf inserted in list, so count is incremented.
        Init_Node(leaves(lastcount).weight, lastcount + 1, pool(oldchain).tail, newchain);
      else
        Init_Node(sum, lastcount, lists(index - 1)(1), newchain);
        if not final then
          --  Two lookahead chains of previous list used up, create new ones.
          Boundary_PM(index - 1, False);
          Boundary_PM(index - 1, False);
        end if;
      end if;
    end if;
  end Boundary_PM;

  --  Initializes each list with as lookahead chains the two leaves with lowest weights.

  procedure Init_Lists is
    node0: constant Index_type:= Get_Free_Node(use_lists => False);
    node1: constant Index_type:= Get_Free_Node(use_lists => False);
  begin
    Init_Node(leaves(0).weight, 1, 0, node0);
    Init_Node(leaves(1).weight, 2, 0, node1);
    lists:= (others => (node0, node1));
  end Init_Lists;

  --  Converts result of boundary package-merge to the bit_lengths. The result in the
  --  last chain of the last list contains the amount of active leaves in each list.
  --  chain: Chain to extract the bit length from (last chain from last list).

  procedure Extract_Bit_Lengths(chain: Index_type) is
    node: Index_type:= chain;
  begin
    while node /= 0 loop
      for i in 0 .. pool(node).count - 1 loop
        bit_lengths(leaves(i).symbol):= bit_lengths(leaves(i).symbol) + 1;
      end loop;
      node := pool(node).tail;
    end loop;
  end Extract_Bit_Lengths;

  -----------------------------------------------------------------------
  -- Generic Sort (standard in Ada 2005+, but this unit is Ada 95 )
  -- Code originally from the Charles library by Matthew J Heaney
  -----------------------------------------------------------------------
  generic
     type Index_Type is (<>);
     type Element_Type is private;
     type Array_Type is array (Index_Type range <>) of Element_Type;
     with function "<" (Left, Right : Element_Type) return Boolean is <>;
  procedure Generic_Quicksort (Container : in out Array_Type);
  --
  procedure Generic_Quicksort (Container : in out Array_Type) is
     function Is_Less (I, J : Index_Type) return Boolean is
        pragma Inline (Is_Less);
     begin
        return Container (I) < Container (J);
     end;
     procedure Swap (I, J : Index_Type) is
        pragma Inline (Swap);
        EI : constant Element_Type := Container (I);
     begin
        Container (I) := Container (J);
        Container (J) := EI;
     end;
     procedure Sort (First, Last : Index_Type'Base) is
        Pivot, Lo, Mid, Hi : Index_Type;
     begin
        if Last <= First then
           return;
        end if;
        Lo := First;
        Hi := Last;
        if Last = Index_Type'Succ (First) then
           if not Is_Less (Lo, Hi) then
              Swap (Lo, Hi);
           end if;
           return;
        end if;
        Mid := Index_Type'Val
                 (Index_Type'Pos (Lo) +
                  (Index_Type'Pos (Hi) - Index_Type'Pos (Lo)) / 2);
        if Is_Less (Lo, Mid) then
           if Is_Less (Lo, Hi) then
              if Is_Less (Mid, Hi) then
                 Swap (Lo, Mid);
              else
                 Swap (Lo, Hi);
              end if;
           else
              null;  --lo is median
           end if;
        elsif Is_Less (Lo, Hi) then
           null; --lo is median
        elsif Is_Less (Mid, Hi) then
           Swap (Lo, Hi);
        else
           Swap (Lo, Mid);
        end if;
        Pivot := Lo;
        Outer :
        loop
           loop
              exit Outer when not (Pivot < Hi);
              if Is_Less (Hi, Pivot) then
                 Swap (Hi, Pivot);
                 Pivot := Hi;
                 Lo := Index_Type'Succ (Lo);
                 exit;
              else
                 Hi := Index_Type'Pred (Hi);
              end if;
           end loop;
           loop
              exit Outer when not (Lo < Pivot);
              if Is_Less (Lo, Pivot) then
                 Lo := Index_Type'Succ (Lo);
              else
                 Swap (Lo, Pivot);
                 Pivot := Lo;
                 Hi := Index_Type'Pred (Hi);
                 exit;
              end if;
           end loop;
        end loop Outer;
        Sort (First, Index_Type'Pred (Pivot));
        Sort (Index_Type'Succ (Pivot), Last);
     end Sort;
  begin
     Sort (Container'First, Container'Last);
  end Generic_Quicksort;

  function "<"(a, b: Leaf_Node) return Boolean is
  begin
    return a.weight < b.weight;
  end;

  procedure QSort is new Generic_Quicksort(Index_type, Leaf_Node, Leaf_array, "<");

begin
  bit_lengths:= (others => 0);
  --  Count used symbols and place them in the leaves.
  for i in frequencies'Range loop
    if frequencies(i) > 0 then
      leaves(num_symbols):= (frequencies(i), i);
      num_symbols:= num_symbols + 1;
    end if;
  end loop;
  --  Check special cases and error conditions.
  if num_symbols > 2 ** max_bits then
    raise Constraint_Error;  --  Error, too few max_bits to represent symbols.
  end if;
  if num_symbols = 0 then
    return;  --  No symbols at all. OK.
  end if;
  if num_symbols = 1 then
    bit_lengths(leaves(0).symbol) := 1;
    return;  --  Only one symbol, give it bit length 1, not 0. OK.
  end if;
  --  Sort the leaves from lightest to heaviest.
  QSort(leaves(0..num_symbols-1));
  for i in 0..num_symbols-1 loop
    if i > 0 and then leaves(i-1).weight > leaves(i).weight then
      raise Constraint_Error;  --  Buggy sorting
    end if;
  end loop;
  Init_Lists;
  --  In the last list, 2 * num_symbols - 2 active chains need to be created. Two
  --  are already created in the initialization. Each Boundary_PM run creates one.
  num_Boundary_PM_runs := 2 * num_symbols - 4;
  for i in 1 .. num_Boundary_PM_runs loop
    Boundary_PM(Index_Type(max_bits - 1), i = num_Boundary_PM_runs);
  end loop;
  Extract_Bit_Lengths(lists(Index_Type(max_bits - 1))(1));
end Length_limited_Huffman_code_lengths;
