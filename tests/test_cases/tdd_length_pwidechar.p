program tdd_length_pwidechar;

{$mode objfpc}

var
  arr: array[0..4] of WideChar;

begin
  arr[0] := 'H';
  arr[1] := 'i';
  arr[2] := #0;
  arr[3] := 'X';
  arr[4] := #0;
  writeln(Length(PWideChar(@arr[0])));
end.
