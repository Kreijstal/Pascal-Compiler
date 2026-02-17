program tdd_types_scopedenums_literals;
{$mode objfpc}
{$scopedenums on}

type
  TA = (Center, Left, Right);
  TB = (Center, Left, Right);

begin
  WriteLn(Ord(TA.Left), ' ', Ord(TB.Left), ' ', Ord(TA.Right), ' ', Ord(TB.Right));
end.
