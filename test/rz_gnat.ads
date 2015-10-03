--  ReZip for GNAT, with Trace-Back on unhandled exception

-- Build with -bargs -E

with TB_Wrap, ReZip;
pragma Elaborate_All(TB_Wrap);

procedure RZ_GNAT is new TB_Wrap(ReZip);