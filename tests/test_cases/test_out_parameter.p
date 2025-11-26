program test_out_parameter;
{$mode objfpc}

{ Test for 'out' parameter modifier - FPC compatible with objfpc mode }

procedure GetValues(out x: integer; out y: integer);
begin
  x := 10;
  y := 20;
end;

procedure SwapValues(var a: integer; out b: integer; c: integer);
begin
  b := a;
  a := c;
end;

var
  a, b, c: integer;

begin
  { Test 1: Simple out parameters }
  GetValues(a, b);
  if (a = 10) and (b = 20) then
    WriteLn(0)
  else
    WriteLn(1);
    
  { Test 2: Mixed parameter modes }
  a := 5;
  c := 15;
  SwapValues(a, b, c);
  if (a = 15) and (b = 5) then
    WriteLn(0)
  else
    WriteLn(1);
end.
