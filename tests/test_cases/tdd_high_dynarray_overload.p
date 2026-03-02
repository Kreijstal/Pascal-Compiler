program tdd_high_dynarray_overload;
{ Test that High() on dynamic arrays works when passed as argument to overloaded function }

function MyFunc(const buf; len: SizeInt; b: Char): SizeInt;
begin
  Result := len;
end;

var
  arr: array of Integer;
  r: SizeInt;
begin
  SetLength(arr, 5);
  r := MyFunc(arr, High(arr) + 1, 'x');
  writeln(r);
end.
