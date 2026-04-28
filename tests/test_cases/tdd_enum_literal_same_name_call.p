program tdd_enum_literal_same_name_call;

uses
  tdd_enum_literal_same_name_unit;

type
  TLocalOp = (opcompare, opaddition);

procedure Check(Value: TLocalOp);
begin
  Writeln(Ord(Value));
end;

begin
  Check(opcompare);
end.
