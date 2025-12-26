program enum_equality;

type
  TSignalState = (ssNotHooked, ssHooked, ssOverridden);

var
  A, B: TSignalState;
begin
  A := ssHooked;
  B := ssNotHooked;
  if A = ssHooked then
    Write('A=Hooked;');
  if B <> ssHooked then
    Write('B<>Hooked;');
  if A = B then
    Write('A=B')
  else
    Write('A<>B');
  WriteLn;
end.
