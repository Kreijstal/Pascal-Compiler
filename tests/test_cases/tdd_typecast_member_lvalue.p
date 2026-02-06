{$mode objfpc}
program tdd_typecast_member_lvalue;

type
  TBase = class
    Value: Integer;
  end;

var
  Obj: TObject;
  Base: TBase;

begin
  Base := TBase.Create;
  Obj := Base;
  TBase(Obj).Value := 7;
  writeln(TBase(Obj).Value);
end.
