{ TDD: Low/High on set types.
  Bug: KGPC only supports Low/High on arrays and strings, not set types.
  FPC allows Low(set of 0..N) = 0, High(set of 0..N) = N.
  Used in FPC compiler for iterating register sets. }
program tdd_fpc_bootstrap_low_high_set_type;

type
  TSmallSet = set of 0..31;
  TByteSet = set of Byte;

var
  i: Integer;
  s: TSmallSet;
  count: Integer;
begin
  { Low/High on set type identifiers }
  WriteLn(Low(TSmallSet));
  WriteLn(High(TSmallSet));

  WriteLn(Low(TByteSet));
  WriteLn(High(TByteSet));

  { Practical use: iterate over set elements }
  s := [1, 5, 10, 31];
  count := 0;
  for i := Low(TSmallSet) to High(TSmallSet) do
    if i in s then
      Inc(count);
  WriteLn(count);
end.
