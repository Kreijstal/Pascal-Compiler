{ Test: Aligned intrinsic function }
{ BUG: Aligned(Pointer, Alignment) should check pointer alignment }
program intrinsic_aligned;
var
  P: Pointer;
  A: Boolean;
begin
  P := Pointer(16);
  A := Aligned(P, 8);
  if A then
    WriteLn(1)
  else
    WriteLn(0);
end.
