program tdd_macro_nested_define;

{$macro on}
{$define SBChar:=AnsiChar}
{$define TSBCharArray:=Array of SBChar}

var
  A: TSBCharArray;

begin
  SetLength(A, 1);
  A[0] := 'Z';
  writeln(A[0]);
end.
