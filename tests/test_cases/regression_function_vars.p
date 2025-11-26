program RegressionTestFunctionVars;

const MaximalAlfa = 20;

type TAlfa = array[1..MaximalAlfa] of char;

function StringCompare(var s1, s2: TAlfa): boolean;
var f: boolean;
    i: integer;
begin
  f := true;
  i := 1;
  while f and (i <= MaximalAlfa) do begin
    f := (s1[i] = s2[i]);
    i := i + 1;
  end;
  StringCompare := f;
end;

var
  a, b: TAlfa;
  result: boolean;
  i: integer;

begin
  // Initialize arrays with spaces
  for i := 1 to MaximalAlfa do begin
    a[i] := ' ';
    b[i] := ' ';
  end;
  
  a[1] := 'H';
  a[2] := 'i';
  b[1] := 'H';
  b[2] := 'i';
  result := StringCompare(a, b);
  
  if result then
    WriteLn(0)
  else
    WriteLn(1);
end.
