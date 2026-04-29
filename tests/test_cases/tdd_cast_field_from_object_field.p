program tdd_cast_field_from_object_field;

type
  TBase = class
    marker: Longint;
  end;

  TDerived = class(TBase)
    value: Longint;
  end;

  THolder = class
    item: TBase;
    procedure SetValue;
    function ReadValue: Longint;
  end;

procedure THolder.SetValue;
begin
  TDerived(item).value := 42;
end;

function THolder.ReadValue: Longint;
begin
  ReadValue := TDerived(item).value;
end;

var
  h: THolder;
  d: TDerived;
begin
  h := THolder.Create;
  d := TDerived.Create;
  d.marker := 7;
  d.value := 3;
  h.item := d;

  h.SetValue;

  WriteLn(d.marker);
  WriteLn(d.value);
  WriteLn(h.ReadValue);
end.
