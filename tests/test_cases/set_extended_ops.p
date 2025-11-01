program SetExtendedOps;
var
  base: set of 1..10;
  toDrop: set of 1..10;
  sym: set of 1..10;
  diff: set of 1..10;
begin
  base := [1, 2, 3, 4, 5, 6];
  toDrop := [3, 5];
  diff := base - toDrop;
  if (1 in diff) and (2 in diff) and not (3 in diff) and (4 in diff) and not (5 in diff) then
    WriteLn('difference-ok')
  else
    WriteLn('difference-fail');

  sym := [1, 3, 5] >< [3, 4, 6];
  if (1 in sym) and not (3 in sym) and (4 in sym) and (5 in sym) and (6 in sym) then
    WriteLn('symdiff-ok')
  else
    WriteLn('symdiff-fail');

  base := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  toDrop := [3, 4];
  base := base - toDrop;
  if (2 in base) and not (3 in base) and not (4 in base) and (5 in base) then
    WriteLn('difference-assign-ok')
  else
    WriteLn('difference-assign-fail');

  WriteLn('Done');
end.
