program tdd_operator_param_shadow_property;
{$mode objfpc}

type
  TUInt24Rec = packed record
  public
    a, b, c: Byte;
    property byte0: Byte read a write a;
    class operator Implicit(a: TUInt24Rec): Byte;
  end;

class operator TUInt24Rec.Implicit(a: TUInt24Rec): Byte;
begin
  Result := a.byte0;
end;

var
  v: TUInt24Rec;
begin
  v.a := 7;
  WriteLn(Byte(v));
end.
