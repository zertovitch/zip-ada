-- Build with -bargs -E

with TB_Wrap, Demo_csv_into_zip;
pragma Elaborate_All(TB_Wrap);

procedure Demo_CSV_GNAT is new TB_Wrap(Demo_csv_into_zip);
