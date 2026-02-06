{$mode objfpc}
program tdd_implicit_overload_no_directive;

type
  TRawRec = record
    v: longint;
  end;

  TUnicodeRec = record
    s: unicodestring;
  end;

procedure FindClose(var r: TRawRec);
begin
  r.v := 1;
  writeln('raw');
end;

procedure FindClose(var u: TUnicodeRec);
begin
  u.s := 'ok';
  writeln('uni');
end;

var
  r: TRawRec;
  u: TUnicodeRec;

begin
  FindClose(r);
  FindClose(u);
end.
