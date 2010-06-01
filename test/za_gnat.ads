-- Build with -bargs -E

with TB_Wrap, ZipAda;
pragma Elaborate_All(TB_Wrap);

procedure ZA_GNAT is new TB_Wrap(ZipAda);