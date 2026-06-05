{ Regression: include/exclude/new/dispose on a set reached by dereferencing an
  array-indexed pointer (`arr[i]^[k]`) — the FPC 3.2.2 rgobj.pas interference-
  bitmap pattern. KGPC used to drop the pointer's target type when dereferencing
  an array-index result, so the set element resolved to LongInt and the
  exclude/include/new checks failed ("target must be a set"). }
program tdd_exclude_include_ptr_array_set;
type
  TArrSet = array[byte] of set of byte;
  PArrSet = ^TArrSet;
  TPtrArr = array[0..3] of PArrSet;
var
  arr: TPtrArr;
  i: integer;
begin
  for i := 0 to 3 do
  begin
    new(arr[i]);
    arr[i]^[0] := [];
  end;
  include(arr[2]^[0], 5);
  include(arr[2]^[0], 9);
  exclude(arr[2]^[0], 9);
  if 5 in arr[2]^[0] then writeln('5 in');
  if not (9 in arr[2]^[0]) then writeln('9 out');
  for i := 0 to 3 do
    dispose(arr[i]);
end.
