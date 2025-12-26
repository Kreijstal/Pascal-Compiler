program typed_const_system_constexpr;

const
  SepOrd = Ord(DirectorySeparator);
  SepPlus = SepOrd + 1;

var
  Arr: array[1..SepPlus] of Integer;

begin
  WriteLn(SepOrd);
  WriteLn(High(Arr));
end.
