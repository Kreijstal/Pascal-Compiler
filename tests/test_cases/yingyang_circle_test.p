{ Test for floating-point comparison bug fix - verifies that real comparisons
  work correctly when operand spilling is needed (e.g., when comparing the result
  of two function calls that return real values). This was a bug where the
  spill/restore code used the binary expression's result type (BOOL) instead of
  the operand's type (REAL), causing 32-bit moves instead of 64-bit for doubles. }
program yingyang_circle_test;
uses Math;

function inCircle(centre_x:Integer;centre_y:Integer;radius:Integer;x:Integer;y:Integer):Boolean;
begin
  inCircle:=power(x-centre_x,2)+power(y-centre_y,2)<=power(radius,2);
end;

function bigCircle(radius:Integer;x:Integer;y:Integer):Boolean;
begin
  bigCircle:=inCircle(0,0,radius,x,y);
end;

function testGreater(a, b: Integer): Boolean;
begin
  testGreater := power(a, 2) <= power(b, 2);
end;

var
  passed: Integer;
begin
  passed := 0;
  
  { Test direct comparisons }
  if not (power(9,2)<=power(8,2)) then passed := passed + 1;  { 81<=64 should be FALSE }
  if not (power(10,2)<=power(8,2)) then passed := passed + 1; { 100<=64 should be FALSE }
  if (power(5,2)<=power(8,2)) then passed := passed + 1;      { 25<=64 should be TRUE }
  if (power(8,2)<=power(8,2)) then passed := passed + 1;      { 64<=64 should be TRUE }
  
  { Test via function returning boolean from comparison }
  if not testGreater(9, 8) then passed := passed + 1;  { 81<=64 should be FALSE }
  if testGreater(5, 8) then passed := passed + 1;      { 25<=64 should be TRUE }
  
  { Test via inCircle function }
  if not bigCircle(8, 9, 0) then passed := passed + 1; { point (9,0) outside radius 8 circle }
  if bigCircle(8, 5, 5) then passed := passed + 1;     { point (5,5) inside radius 8 circle }
  if bigCircle(8, 0, 0) then passed := passed + 1;     { center point always inside }
  if bigCircle(8, 8, 0) then passed := passed + 1;     { point on circle boundary }
  
  writeln(passed);
end.
