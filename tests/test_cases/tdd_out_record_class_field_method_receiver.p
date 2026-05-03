program tdd_out_record_class_field_method_receiver;
{$mode objfpc}

type
  TItem = class
    Value: LongInt;
    constructor Create(AValue: LongInt; Flag: Boolean);
    destructor Destroy; override;
  end;

  THolder = record
    NewItem: TItem;
    OldItem: TItem;
    Valid: Boolean;
  end;

constructor TItem.Create(AValue: LongInt; Flag: Boolean);
begin
  inherited Create;
  if Flag then
    Value := AValue
  else
    Value := 0;
end;

destructor TItem.Destroy;
begin
  WriteLn(Value);
  inherited Destroy;
end;

procedure Replace(out Holder: THolder);
begin
  Holder.OldItem := nil;
  Holder.Valid := True;
  Holder.NewItem := TItem.Create(7, True);
end;

procedure Restore(const Holder: THolder);
begin
  if Holder.Valid then
    Holder.NewItem.Free;
end;

var
  Holder: THolder;

begin
  Replace(Holder);
  Restore(Holder);
end.
