with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body PPMd is

  --
  --  From: See2Context.cs
  --  SEE2 (secondary escape estimation) contexts for PPM contexts with masked symbols.
  --

  procedure Initialize (self : in out See2Context; initialValue : uint) is
  begin
    self.Shift   := PeriodBitCount - 4;
    self.Summary := ushort (Shift_Left (initialValue, Integer (self.Shift)));
    self.Count   := 7;
  end Initialize;

  procedure Mean (self : in out See2Context; result : out uint) is
    value : uint := uint (Shift_Right (self.Summary, Integer (self.Shift)));
  begin
    self.Summary := ushort (uint (self.Summary) - value);
    result := uint (value);
    if value = 0 then
      result := result + 1;
    end if;
  end Mean;

  procedure Update (self : in out See2Context) is
  begin
    if self.Shift < PeriodBitCount then
      self.Count := self.Count - 1;
      if self.Count = 0 then
        self.Summary := self.Summary + self.Summary;
        self.Count   := Shift_Left (3, Integer (self.Shift));
        self.Shift   := self.Shift + 1;
      end if;
    end if;
  end Update;

begin
  PpmContext_Zero.Address := 0;
end PPMd;
