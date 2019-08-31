package body PPMd is

  procedure Ppmd_See_Update (p : in out CPpmd_See) is
  begin
    if p.Shift < PPMD_PERIOD_BITS then
      p.Count := p.Count - 1;  --  Likely to loop from 0 to 255, 254, 253, ...
      if p.Count = 0 then
        p.Summ  := Shift_Left (p.Summ, 1);
        p.Count := Shift_Left (3, Natural (p.Shift));  --  = 0 when p.Shift > 7
        p.Shift := p.Shift + 1;
      end if;
    end if;
  end Ppmd_See_Update;

end PPMd;
