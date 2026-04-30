program tdd_classref_ctor_statement_vmt;
{$mode objfpc}

type
  TInner = class
    Value: LongInt;
    constructor Create(AValue: LongInt); virtual;
  end;

  TInnerClass = class of TInner;

constructor TInner.Create(AValue: LongInt);
begin
  inherited Create;
  Value := AValue;
  WriteLn(Value);
end;

var
  CInner: TInnerClass = TInner;

begin
  CInner.Create(13);
end.
