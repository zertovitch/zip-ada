--  Package to construct and manipulate suffix arrays

--  Authors: Rob Irving, Lorna Love
--  Final version: 24 January 2001

--  This version constructs the suffix array by means of
--  the refined version of the suffix binary search tree
--  represented using an array of nodes

with Ada.Text_IO;

package body Suffix_Arrays is

  type Node_Type is record
    Lcp              : Count_Type;
    Sub              : Dirn_Type;
    L_Child, R_Child : Count_Type;
    Lex_Closest      : Count_Type;
  end record;

  type Tree_Type is array (Count_Type range <>) of Node_Type;

  --------------------------------------------------------------------

  procedure Output_Sa (Sa : in Sa_Type; F_Name : in String) is
    use Ada.Text_IO;
    package CIO is new Integer_IO (Count_Type);
    F : File_Type;
  begin
    Create (F, Mode => Out_File, Name => F_Name);
    for I in Sa'Range loop
      CIO.Put (F, Sa (I).Suffix, Width => 3);
      CIO.Put (F, Sa (I).Lcp, Width => 3);
      if Sa (I).Dir = Left then
        Put (F, " Left");
      else
        Put (F, " Right");
      end if;
      New_Line (F);
    end loop;
    Close (F);
  end Output_Sa;

  --------------------------------------------------------------------

  function Suffix (Sa : in Sa_Type; Index : Count_Type) return Count_Type is
  (Sa (Index).Suffix);

  --------------------------------------------------------------------

  function Size (T : in Tree_Type) return Count_Type is
    --  Returns the number of nodes in the tree T

    Count : Count_Type := 0;

    procedure Rec_Traverse (I : in Count_Type) is
    begin
      if I /= 0 then
        Rec_Traverse (T (I).L_Child);
        Rec_Traverse (T (I).R_Child);
        Count := Count + 1;
      end if;
    end Rec_Traverse;

  begin
    Rec_Traverse (1);
    return Count;
  end Size;

  pragma Unreferenced (Size);

  --------------------------------------------------------------------

  procedure Insert_In_Sbst (T : in out Tree_Type; S : in Symbol_Array; J : in Count_Type)
  is

    --  Inserts the suffix S (J .. len (S)) into T, the SBST for S

    --------------------------------------------------------------------

    procedure Branch_Left (I : in out Count_Type; D : out Dirn_Type) with Inline
    is
    begin
      I := T (I).L_Child;
      D := Left;
    end Branch_Left;

    --------------------------------------------------------------------

    procedure Branch_Right (I : in out Count_Type; D : out Dirn_Type) with Inline
    is
    begin
      I := T (I).R_Child;
      D := Right;
    end Branch_Right;

    --------------------------------------------------------------------

    K, M, N                             : Count_Type;
    Llcp, Rlcp                          : Count_Type := 0;
    Dir                                 : Dirn_Type;
    I, Prev_I, Start_Posn, Closest_Link : Count_Type;

  begin
    if T (J - 1).Lcp <= 1 then  --  No useful info, so just start at the root
      Start_Posn := 1;
      Prev_I     := Start_Posn;  --  <- Added by GdM (uninitialized variable detected).
    else
      Start_Posn := T (J - 1).Lex_Closest + 1;
      if T (Start_Posn).Lcp < T (J - 1).Lcp - 1
      then  --  insert in this subtree
        Prev_I := Start_Posn;
        if T (J - 1).Sub = Left then
          Branch_Left (Start_Posn, Dir);
          Llcp := T (J - 1).Lcp - 1;
        else
          Branch_Right (Start_Posn, Dir);
          Rlcp := T (J - 1).Lcp - 1;
        end if;
      else
        while T (Start_Posn).Lcp >= T (J - 1).Lcp - 1 loop
          Start_Posn := T (Start_Posn).Lex_Closest;
        end loop;
        M := T (J - 1).Lcp - 1;
        while J + M <= S'Length and then S (J + M) = S (Start_Posn + M) loop
          M := M + 1;
        end loop;
        Prev_I := Start_Posn;
        if J + M > S'Length or else S (J + M) < S (Start_Posn + M) then
          Llcp := M;
          Branch_Left (Start_Posn, Dir);
        else
          Rlcp := M;
          Branch_Right (Start_Posn, Dir);
        end if;
      end if;
    end if;

    I            := Start_Posn;
    Closest_Link := Prev_I;
    M            := S'Length - J + 1;
    while I /= 0 loop
      Prev_I := I;
      if T (I).Lcp > Count_Type'Max (Llcp, Rlcp) then
        if T (I).Sub = Left then
          Branch_Left (I, Dir);
        else
          Branch_Right (I, Dir);
        end if;
      elsif Llcp > Rlcp then
        if Llcp > T (I).Lcp then
          if T (I).Sub = Left then
            Rlcp := T (I).Lcp;
            if Rlcp >= Llcp then
              Closest_Link := I;
            end if;
          end if;
          Branch_Right (I, Dir);
        elsif Llcp = T (I).Lcp and T (I).Sub = Right then
          Branch_Right (I, Dir);
        end if;
      elsif Rlcp > Llcp then
        if Rlcp > T (I).Lcp then
          if T (I).Sub = Right then
            Llcp := T (I).Lcp;
            if Llcp >= Rlcp then
              Closest_Link := I;
            end if;
          end if;
          Branch_Left (I, Dir);
        elsif Rlcp = T (I).Lcp and T (I).Sub = Left then
          Branch_Left (I, Dir);
        end if;
      end if;
      if I = Prev_I then  --  haven't yet branched, so compare characters
        K := T (I).Lcp;
        N := S'Length - I + 1;
        while K < Count_Type'Min (M, N) and then S (J + K) = S (I + K) loop
          K := K + 1;
        end loop;
        if K = M then
          Llcp := K;
          if Llcp >= Rlcp then
            Closest_Link := I;
          end if;
          Branch_Left (I, Dir);
        elsif K = N then
          Rlcp := K;
          if Rlcp >= Llcp then
            Closest_Link := I;
          end if;
          Branch_Right (I, Dir);
        elsif S (J + K) < S (I + K) then
          Llcp := K;
          if Llcp >= Rlcp then
            Closest_Link := I;
          end if;
          Branch_Left (I, Dir);
        else
          Rlcp := K;
          if Rlcp >= Llcp then
            Closest_Link := I;
          end if;
          Branch_Right (I, Dir);
        end if;
      end if;
    end loop;
    if Dir = Left then
      if Llcp > Rlcp then
        T (Prev_I).L_Child := J;
        T (J)              := (Llcp, Left, 0, 0, Closest_Link);
      else
        T (Prev_I).L_Child := J;
        T (J)              := (Rlcp, Right, 0, 0, Closest_Link);
      end if;
    elsif Llcp > Rlcp then
      T (Prev_I).R_Child := J;
      T (J)              := (Llcp, Left, 0, 0, Closest_Link);
    else
      T (Prev_I).R_Child := J;
      T (J)              := (Rlcp, Right, 0, 0, Closest_Link);
    end if;
  end Insert_In_Sbst;

  --------------------------------------------------------------------

  procedure Build_Sbst (S : in Symbol_Array; T : out Tree_Type) is

  begin
    pragma Assert (S'First = 1, "Array S must have 1 as first index");
    pragma Assert (T'First = 1, "Array T must have 1 as first index");
    T (1) := (0, Right, 0, 0, 0);
    for K in Count_Type range 2 .. S'Length loop
      Insert_In_Sbst (T, S, K);
    end loop;
  end Build_Sbst;

  --------------------------------------------------------------------

  procedure Build_Gen_Sbst (S1, S2 : in Symbol_Array; T : out Tree_Type) is
  begin
    Build_Sbst (S1 & Unused_Symbol & S2, T);
  end Build_Gen_Sbst;

  --------------------------------------------------------------------

  procedure Sbst_To_Isa (T : in Tree_Type; A : out Sa_Type) is
    --  Transform suffix binary search tree T to
    --  'intermediate' form of suffix array A

    I    : Count_Type := 0;  --  inorder position of current node
    Pred : Count_Type;       --  inorder predecessor of current node
    Anc  : Count_Type;       --  'best' ancestor of Pred

    procedure Traverse (J, K : in Count_Type) is
    --  traverse the subtree rooted at T(J);
    --  K is the 'best' ancestor of J, i.e. J's nearest ancestor such that
    --  the sub value of K is undefined, or
    --  the sub value of K is Left and J is in the Right subtree of K, or
    --  the sub value of K is Right and J is in the Left subtree of K
    begin
      if J /= 0 then
        if T (J).Lcp = 0 then
          Traverse (T (J).L_Child, J);
        elsif T (J).Sub = Right then
          Traverse (T (J).L_Child, J);
        else
          Traverse (T (J).L_Child, K);
        end if;
        I            := I + 1;
        A (I).Suffix := J;
        if I > 1 then
          if T (J).L_Child = 0 then
            if T (J).Sub = Right then
              A (I - 1).Lcp := T (J).Lcp;
            else
              A (I - 1).Lcp := T (K).Lcp;
            end if;
          else
            if T (Pred).Sub = Left then
              A (I - 1).Lcp := T (Pred).Lcp;
            else
              A (I - 1).Lcp := T (Anc).Lcp;
            end if;
          end if;
        end if;
        Pred := J;
        Anc  := K;
        if T (J).Lcp = 0 then
          Traverse (T (J).R_Child, J);
        elsif T (J).Sub = Left then
          Traverse (T (J).R_Child, J);
        else
          Traverse (T (J).R_Child, K);
        end if;
      end if;
    end Traverse;

  begin
    Traverse (1, 0);
    A (A'Last).Lcp := 0;
  end Sbst_To_Isa;

  --------------------------------------------------------------------

  procedure Isa_To_Sa (A : in out Sa_Type) is
    --  transform 'intermediate' form of
    --  suffix array A to final form

    procedure Process (Low, High : in Count_Type) is
      Mid, Temp : Count_Type;
    begin
      if High - Low > 1 then
        Mid := (Low + High) / 2;
        Process (Low, Mid);
        Process (Mid, High);
        if Low = A'First - 1 then
          A (Mid).Dir := Left;
        elsif High = A'Last + 1 or else A (Low).Lcp > A (Mid).Lcp then
          Temp        := A (Low).Lcp;
          A (Low).Lcp := A (Mid).Lcp;
          A (Mid).Lcp := Temp;
          A (Mid).Dir := Right;
        else
          A (Mid).Dir := Left;
        end if;
      end if;
    end Process;

  begin
    Process (A'First - 1, A'Last + 1);
  end Isa_To_Sa;

  --------------------------------------------------------------

  procedure Build_Sa (S : in Symbol_Array; A : out Sa_Type) is

    T : Tree_Type (1 .. S'Length);

  begin
    Build_Sbst (S, T);
    Sbst_To_Isa (T, A);
    Isa_To_Sa (A);
  end Build_Sa;

  --------------------------------------------------------------------

  procedure Build_Gen_Sa (S1, S2 : in Symbol_Array; A : out Sa_Type) is
    T : Tree_Type (1 .. S1'Length + S2'Length + 1);
  begin
    Build_Gen_Sbst (S1, S2, T);
    Sbst_To_Isa (T, A);
    Isa_To_Sa (A);
  end Build_Gen_Sa;

  --------------------------------------------------------------------

  procedure Search_Left_and_Rightmost
   (A : in Sa_Type; S, X : in Symbol_Array; Pos_L, Pos_R : out Count_Type)
  is

    --  Searches the suffix array A, of Element_Array S, for a target
    --  Element_Array X; returns the leftmost position in A of a suffix that has
    --  X as a prefix, or zero if X is not a substring of S

    F, Lt, Rt, Mid, Limit, N, K, Pos, Temp_L, Temp_R : Count_Type;
    Found                                            : Boolean := False;
    L, Llcp, Rlcp                                    : Count_Type := 0;
    D                                                : Dirn_Type;

  begin
    Found := False;
    Pos   := 0;
    Lt    := A'First - 1;
    Rt    := A'Last + 1;
    while not Found and Rt - Lt > 1 loop
      Mid := (Lt + Rt) / 2;
      D   := A (Mid).Dir;
      L   := A (Mid).Lcp;
      if L > Count_Type'Max (Llcp, Rlcp) then
        if D = Left then
          Rt := Mid;
        else
          Lt := Mid;
        end if;
      elsif Llcp > Rlcp then
        if Llcp > L then
          if D = Left then
            Rlcp := L;
          end if;
          Lt := Mid;
        elsif Llcp = L and D = Right then
          Lt := Mid;
        end if;
      elsif Rlcp > Llcp then
        if Rlcp > L then
          if D = Right then
            Llcp := L;
          end if;
          Rt := Mid;
        elsif Rlcp = L and D = Left then
          Rt := Mid;
        end if;
      end if;
      if Lt /= Mid and Rt /= Mid then  --  need to compare characters
        F     := A (Mid).Suffix;
        K     := L;
        N     := S'Length - F + 1;
        Limit := Count_Type'Min (X'Length, N);
        while K < Limit and then S (F + K) = X (K + X'First) loop
          K := K + 1;
        end loop;
        if K = X'Length then
          Found := True;
          Pos   := Mid;
        elsif K = N or else S (F + K) < X (K + X'First) then
          Rlcp := K;
          Lt   := Mid;
        else
          Llcp := K;
          Rt   := Mid;
        end if;
      end if;
    end loop;
    Pos_L := Pos;
    Pos_R := Pos;
    if Pos /= 0 then
      Temp_L := Mid;
      Temp_R := Rt;
      Rt     := Mid;
      while Rt - Lt > 1 loop
        Mid := (Lt + Rt) / 2;
        D   := A (Mid).Dir;
        L   := A (Mid).Lcp;
        if L < X'Length or else D = Right then
          Lt := Mid;
        else
          Pos_L := Mid;
          Rt    := Mid;
        end if;
      end loop;
      Rt := Temp_R;
      Lt := Temp_L;
      while Rt - Lt > 1 loop
        Mid := (Lt + Rt) / 2;
        D   := A (Mid).Dir;
        L   := A (Mid).Lcp;
        if L < X'Length or else D = Left then
          Rt := Mid;
        else
          Pos_R := Mid;
          Lt    := Mid;
        end if;
      end loop;
    end if;
  end Search_Left_and_Rightmost;

  --------------------------------------------------------------------

  procedure Search_Sa
   (A : in Sa_Type; S, X : in Symbol_Array; Found : out Boolean; Pos : out Count_Type)
  is
    Pos_L, Pos_R : Count_Type;
  begin
    Search_Left_and_Rightmost (A, S, X, Pos_L, Pos_R);
    if Pos_L = 0 then
      Found := False;
    else
      Found := True;
      Pos   := A (Pos_L).Suffix;
    end if;
  end Search_Sa;

  --------------------------------------------------------------------

  function Fully_Search_Sa
   (A : in Sa_Type; S, X : in Symbol_Array) return Count_Array
  is
    Pos_L, Pos_R : Count_Type;
  begin
    Search_Left_and_Rightmost (A, S, X, Pos_L, Pos_R);
    if Pos_L > 0 then
      declare
        Pos_List : Count_Array (Pos_L .. Pos_R);
      begin
        for I in Pos_L .. Pos_R loop
          Pos_List (I) := A (I).Suffix;
        end loop;
        return Pos_List;
      end;
    else
      declare
        Empty : Count_Array (1 .. 0);
      begin
        return Empty;
      end;
    end if;
  end Fully_Search_Sa;

  --------------------------------------------------------------------

  function Num_Occurrences (A : Sa_Type; S, X : Symbol_Array) return Count_Type is
    Pos_L, Pos_R : Count_Type;
  begin
    Search_Left_and_Rightmost (A, S, X, Pos_L, Pos_R);
    if Pos_L > 0 then
      return Pos_R - Pos_L + 1;
    else
      return 0;
    end if;
  end Num_Occurrences;

  --------------------------------------------------------------------

  procedure Traverse_For_Lrs (A : in Sa_Type; Pos1, Pos2, Len : out Count_Type) is

    procedure Rec_Traverse (Low, High : in Count_Type) is
      Mid : Count_Type;
    begin
      if High - Low > 1 then
        Mid := (Low + High) / 2;
        Rec_Traverse (Low, Mid);
        Rec_Traverse (Mid, High);
        if A (Mid).Lcp > Len then
          Len  := A (Mid).Lcp;
          Pos2 := A (Mid).Suffix;
          if A (Mid).Dir = Left then
            Pos1 := A (High).Suffix;
          else
            Pos1 := A (Low).Suffix;
          end if;
        end if;
      end if;
    end Rec_Traverse;

  begin
    Len := 0;
    Rec_Traverse (A'First - 1, A'Last + 1);
  end Traverse_For_Lrs;

  --------------------------------------------------------------------

  procedure Traverse_For_Lcs
   (A : in Sa_Type; Len_S1 : in Count_Type; Pos1, Pos2, Len : out Count_Type)
  is

    procedure Rec_Traverse (Low, High : in Count_Type) is
      Mid : Count_Type;
    begin
      if High - Low > 1 then
        Mid := (Low + High) / 2;
        Rec_Traverse (Low, Mid);
        Rec_Traverse (Mid, High);
        if A (Mid).Lcp > Len then
          if A (Mid).Dir = Left then
            if A (Mid).Suffix in 1 .. Len_S1
              and then A (High).Suffix not in 1 .. Len_S1
            then
              Len  := A (Mid).Lcp;
              Pos1 := A (Mid).Suffix;
              Pos2 := A (High).Suffix - Len_S1 - 1;
            elsif A (Mid).Suffix not in 1 .. Len_S1
              and then A (High).Suffix in 1 .. Len_S1
            then
              Len  := A (Mid).Lcp;
              Pos2 := A (Mid).Suffix - Len_S1 - 1;
              Pos1 := A (High).Suffix;
            end if;
          else
            if A (Mid).Suffix in 1 .. Len_S1
              and then A (Low).Suffix not in 1 .. Len_S1
            then
              Len  := A (Mid).Lcp;
              Pos1 := A (Mid).Suffix;
              Pos2 := A (Low).Suffix - Len_S1 - 1;
            elsif A (Mid).Suffix not in 1 .. Len_S1
              and then A (Low).Suffix in 1 .. Len_S1
            then
              Len  := A (Mid).Lcp;
              Pos2 := A (Mid).Suffix - Len_S1 - 1;
              Pos1 := A (Low).Suffix;
            end if;
          end if;
        end if;
      end if;
    end Rec_Traverse;

  begin
    Len := 0;
    Rec_Traverse (A'First - 1, A'Last + 1);
  end Traverse_For_Lcs;

end Suffix_Arrays;
