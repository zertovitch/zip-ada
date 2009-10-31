-- Build with -bargs -E

with TB_Wrap, UnZipAda;
pragma Elaborate_All(TB_Wrap);

procedure UZA_GNAT is new TB_Wrap(UnZipAda);