program tdd_inherited_constructor_chain;
{$mode objfpc}

type
  TBase = class
    value: LongInt;
    constructor Load(x: LongInt);
  end;

  TChild = class(TBase)
    constructor LoadChild(x: LongInt);
  end;

constructor TBase.Load(x: LongInt);
begin
  inherited Create;
  value := x;
end;

constructor TChild.LoadChild(x: LongInt);
begin
  inherited Load(x);
end;

var
  c: TChild;
begin
  c := TChild.LoadChild(42);
  if c <> nil then
    Writeln('allocated');
  Writeln('value=', c.value);
  c.Free;
  Writeln('freed');
end.
