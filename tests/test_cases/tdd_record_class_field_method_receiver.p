program tdd_record_class_field_method_receiver;
{$mode objfpc}

type
  TItem = class
    Value: LongInt;
    procedure SetValue(AValue: LongInt);
    procedure PrintValue;
    destructor Destroy; override;
  end;

  THolder = record
    Item: TItem;
  end;

procedure TItem.SetValue(AValue: LongInt);
begin
  Value := AValue;
end;

procedure TItem.PrintValue;
begin
  WriteLn(Value);
end;

destructor TItem.Destroy;
begin
  WriteLn(Value + 1);
  inherited Destroy;
end;

procedure UseHolder(const Holder: THolder);
begin
  Holder.Item.SetValue(42);
  Holder.Item.PrintValue;
  Holder.Item.Free;
end;

var
  Holder: THolder;

begin
  Holder.Item := TItem.Create;
  UseHolder(Holder);
end.
