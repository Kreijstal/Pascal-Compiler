program tdd_unit_qualified_global_vs_self_field;
{ Regression: inside a method whose class has a field with the same name as a
  unit-global variable, an EXPLICIT unit qualifier (Unit.var) must bind to the
  unit global, not to the same-named Self field.  This is FPC's
  raatt.GetToken `c := scanner.c` where tasmreader also declares field `c`;
  KGPC used to resolve scanner.c to self.c (uninitialised #0), breaking the
  inline-asm reader and blocking the FPC 3.2.2 system.pp bootstrap. }
{$mode objfpc}{$H-}
uses tdd_unit_qualified_global_vs_self_field_unit;

type
  TReader = class
    c: char; { field shadows nothing — but its name collides with the unit global }
    procedure Get;
  end;

procedure TReader.Get;
begin
  { Must read the unit global (set to 'K' below), NOT self.c (set to '?'). }
  c := tdd_unit_qualified_global_vs_self_field_unit.c;
end;

var
  s: TScan;
  r: TReader;
begin
  s := TScan.create;
  s.SetCh('K');
  r := TReader.create;
  r.c := '?';
  r.Get;
  writeln(r.c);
  s.free;
  r.free;
end.
