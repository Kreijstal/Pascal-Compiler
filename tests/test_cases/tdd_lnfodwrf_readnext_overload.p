{$mode objfpc}
program tdd_lnfodwrf_readnext_overload;

{ TDD repro: calling a parameterless method overload when another overload
  with parameters exists.  Pattern from lnfodwrf.pp:
    abbrevs.Children[nr] := er.ReadNext;
  where ReadNext has two overloads:
    function ReadNext: longint;           (no params)
    function ReadNext(var dest; size: SizeInt): Boolean;  (two params)
}

type
  SizeInt = Int64;

  TEReader = object
    val: longint;
    function ReadNext: longint;
    function ReadNext(var dest; size: SizeInt): Boolean;
  end;

function TEReader.ReadNext: longint;
begin
  ReadNext := val;
end;

function TEReader.ReadNext(var dest; size: SizeInt): Boolean;
var
  p: PByte;
begin
  p := @dest;
  p^ := 99;
  ReadNext := true;
end;

var
  er: TEReader;
  x: longint;
begin
  er.val := 42;
  x := er.ReadNext;
  if x = 42 then
    writeln('ok')
  else
    writeln('FAIL: got ', x);
end.
