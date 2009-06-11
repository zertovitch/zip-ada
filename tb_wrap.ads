------------------------------------------------------------------------------
--  File:            TB_Wrap.ads
--  Description:     Trace-back wrapper for GNAT 3.13p+ (spec.)
------------------------------------------------------------------------------

generic

  with procedure My_main_procedure;

procedure TB_Wrap;
