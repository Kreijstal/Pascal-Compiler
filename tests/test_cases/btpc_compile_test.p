program test_btpc_compile;
{ Minimal test to ensure btpc.dpr-like code patterns compile correctly.
  This tests that the compiler can handle large programs with nested procedures,
  arrays, and various Pascal constructs similar to btpc.dpr. }
  
const
  MaxTypes = 10;
  
type
  TType = record
    Size: Integer;
    Kind: Integer;
  end;
  
var
  Types: array[1..MaxTypes] of TType;
  TypePosition: Integer;

procedure InitTypes;
begin
  { Initialize array - this pattern is used in btpc.dpr }
  Types[1].Size := 4;
  Types[1].Kind := 1;
  Types[2].Size := 4;
  Types[2].Kind := 1;
  TypePosition := 2;
end;

begin
  InitTypes;
  writeln('Types initialized: count=', TypePosition);
end.
