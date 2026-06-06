unit tdd_unit_qualified_global_vs_self_field_unit;
{ Mirrors FPC's scanner unit: a unit-global char `c` written through a class
  method, read qualified (scanner.c) from another unit's method whose class
  also has a field named `c` (tasmreader.c in raatt.GetToken). }
{$mode objfpc}{$H-}
interface

type
  TScan = class
    procedure SetCh(x: char);
  end;

var
  c: char;

implementation

procedure TScan.SetCh(x: char);
begin
  c := x; { writes the unit global unqualified, like tscannerfile.readchar }
end;

end.
