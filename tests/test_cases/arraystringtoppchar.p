program arraystringtoppchar;
{ Test ArrayStringToPPchar intrinsic }

var
  arr: array of AnsiString;
  pp: PPAnsiChar;
begin
  SetLength(arr, 3);
  arr[0] := 'hello';
  arr[1] := 'world';
  arr[2] := 'test';

  pp := ArrayStringToPPchar(arr, 0);
  writeln('non-nil: ', pp <> nil);
  writeln(pp[0]);
  writeln(pp[1]);
  writeln(pp[2]);
  writeln('null-term: ', pp[3] = nil);
end.
