{ Test Exit() with return value - required for FPC system.pp bootstrap }
{ KGPC GAP: This test documents a feature not yet supported by KGPC. }
{ FPC allows Exit(value) as a shorthand for setting result and returning }
{ See docs/FPC_BOOTSTRAP_GAPS.md for details. }
program TestExitWithValue;

function GetFive: Integer;
begin
  Exit(5);  { Should return 5 and exit function immediately }
  { This line should never execute - Exit terminates the function }
  WriteLn('This should not print');
end;

function ConditionalReturn(x: Integer): Integer;
begin
  if x > 0 then
    Exit(x * 2);  { Exit with value }
  Exit(-1);  { Default return }
end;

var
  a, b, c: Integer;
begin
  a := GetFive;
  WriteLn(a);
  
  b := ConditionalReturn(7);
  WriteLn(b);
  
  c := ConditionalReturn(-3);
  WriteLn(c);
end.
