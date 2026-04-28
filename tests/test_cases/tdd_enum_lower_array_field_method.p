program tdd_enum_lower_array_field_method;

type
  TSide = (none, caller, callee);

  TSlot = record
    value: LongInt;
    procedure Init(v: LongInt);
  end;

  TBox = object
    prefix: LongInt;
    slots: array[caller..callee] of TSlot;
    suffix: LongInt;
    procedure Fill;
    procedure Print;
  end;

procedure TSlot.Init(v: LongInt);
begin
  value := v;
end;

procedure TBox.Fill;
begin
  prefix := 77;
  suffix := 99;
  slots[caller].Init(11);
  slots[callee].Init(22);
end;

procedure TBox.Print;
begin
  Writeln(prefix);
  Writeln(slots[caller].value);
  Writeln(slots[callee].value);
  Writeln(suffix);
end;

var
  box: TBox;

begin
  box.Fill;
  box.Print;
end.
