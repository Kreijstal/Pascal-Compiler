program ABIBugTest;
{ Minimal test to reproduce ABI argument passing bug }

type
  TString = array[1..20] of char;

procedure TestProc(s: TString; a, b: integer);
var
  i: integer;
begin
  { Access the parameters to ensure they're not optimized away }
  i := a + b;
  if i > 0 then
    writeln('OK');
end;

var
  str: TString;
begin
  str[1] := 'X';
  TestProc(str, 1, 2);
end.
