{ Test: Basic unit compilation }
{ This tests that KGPC can compile a simple unit }
unit unit_compilation_basic;

{$mode objfpc}

interface

var
  UnitVar: Integer;

function UnitFunc(x: Integer): Integer;

implementation

function UnitFunc(x: Integer): Integer;
begin
  UnitFunc := x * 2;
end;

end.
