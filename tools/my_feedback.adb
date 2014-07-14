------------------------------------------------------------------------------
--  File:            My_feedback.adb
--  Description:     part of UnZipAda demo
------------------------------------------------------------------------------

with My_dots;                           use My_dots;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure My_feedback(
      percents_done:  in Natural;
      entry_skipped:  in Boolean;
      user_abort   : out Boolean
    )
is
  new_done_dots: constant Natural:= (dots * percents_done) / 100;
begin
  if entry_skipped then
    Put("-skipped-");
  else
    for i in done_dots+1 .. new_done_dots loop
      if i=1 then
        Put('[');
      end if;
      Put('.');
      if i=dots then
        Put(']');
      end if;
    end loop;
    done_dots:= new_done_dots;
  end if;
  user_abort:= False; -- pointless in this command-line version (Ctrl-C is ok)
end My_feedback;
