program reg_named_arg_equals;

{$mode objfpc}

function Sum(A: Integer; B: Integer = 3; C: Integer = 5): Integer;
begin
  Result := A + B + C;
end;

begin
  writeln(Sum(1));
  writeln(Sum(B=4, A=2));
  writeln(Sum(A=1, C=7));
end.
