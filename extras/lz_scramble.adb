with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

function LZ_Scramble(length: Positive) return String is
  subtype Soup_characters is Character range '!'..'~';
  package Letter_soup is new Ada.Numerics.Discrete_Random(Soup_characters);
  use Letter_soup, Ada.Numerics.Float_Random;
  cg: Letter_soup.Generator;
  fg: Ada.Numerics.Float_Random.Generator;
  res: String(1..length);
  d, l: Integer;
begin
  Reset(cg);
  for i in res'Range loop
    res(i):= Random(cg);
  end loop;
  Reset(fg);
  delay Duration(0.001 * (1.0+Random(fg)));
  for redo in 1..3 loop
    for i in res'Range loop
      if Random(fg) > 0.8 then
        d:= Integer((Random(fg)-0.5)*Float(length));
        l:= Integer((Random(fg))*Float(abs d)*0.9);
        for j in 0..l-1 loop
          if i+j in res'Range and i+d+j in res'Range then
            res(i+j):= res(i+d+j);
          end if;
        end loop;
      end if;
    end loop;
  end loop;
  return res;
end LZ_Scramble;
