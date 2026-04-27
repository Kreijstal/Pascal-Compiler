program tdd_pointer_alias_overload_resolution;

type
  PAnsi = ^AnsiString;
  PShort = ^ShortString;

procedure Pick(p: PAnsi); overload;
begin
  WriteLn('ansi');
end;

procedure Pick(p: PShort); overload;
begin
  WriteLn('short');
end;

var
  p: PAnsi;

begin
  New(p);
  p^ := 'abc';
  Pick(p);
  Dispose(p);
end.
