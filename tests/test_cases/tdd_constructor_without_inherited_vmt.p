program tdd_constructor_without_inherited_vmt;
{$mode objfpc}

type
  TItem = class
    Value: LongInt;
    constructor Create(AValue: LongInt);
    destructor Destroy; override;
  end;

constructor TItem.Create(AValue: LongInt);
begin
  Value := AValue;
end;

destructor TItem.Destroy;
begin
  WriteLn(Value);
  inherited Destroy;
end;

var
  Item: TItem;

begin
  Item := TItem.Create(9);
  Item.Free;
end.
