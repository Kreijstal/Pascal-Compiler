program test_anon_enum_record_field;
{$mode objfpc}
type
  TVariant = packed record
    kind: (vInteger, vString, vNone);
    case Integer of
      0: (iVal: Integer);
      1: (sVal: PChar);
  end;

procedure PrintVariant(const v: TVariant);
begin
  case v.kind of
    vInteger: WriteLn('Int: ', v.iVal);
    vString:  WriteLn('Str');
    vNone:    WriteLn('None');
  end;
end;

var
  a, b, c: TVariant;
begin
  a.kind := vInteger;
  a.iVal := 42;
  b.kind := vString;
  b.sVal := nil;
  c.kind := vNone;
  PrintVariant(a);
  PrintVariant(b);
  PrintVariant(c);
  if a.kind = vInteger then
    WriteLn('OK');
end.
