program tdd_untyped_pointer_variant_field_access;

type
  PInit = ^TInit;
  TInit = record
    dummy: longint;
  end;

  PFull = ^TFull;
  TFull = record
    case integer of
      0: (InitTable: Pointer);
  end;

function GetInit(p: PInit): PInit;
begin
  GetInit := PFull(p)^.InitTable;
end;

var
  r: TFull;
  p, q: PInit;

begin
  p := PInit(@r);
  r.InitTable := p;
  q := GetInit(p);
  if q = p then
    writeln('ok')
  else
    writeln('bad');
end.
