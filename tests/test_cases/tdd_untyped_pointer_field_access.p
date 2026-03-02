program tdd_untyped_pointer_field_access;

type
  PRec = ^TRec;
  TRec = record
    InitTable: Pointer;
  end;

function GetInit(p: PRec): PRec;
begin
  GetInit := p^.InitTable;
end;

var
  r: TRec;
  p, q: PRec;

begin
  r.InitTable := @r;
  p := @r;
  q := GetInit(p);
  if q = p then
    writeln('ok')
  else
    writeln('bad');
end.
