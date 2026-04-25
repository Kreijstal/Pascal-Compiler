program regr_pshortstring_ansistring_assign;

{$mode objfpc}

uses sysutils;

type
  PShortString = ^ShortString;

var
  buf: ShortString;
  p: PShortString;
  s: AnsiString;
begin
  s := 'Hello';
  p := @buf;
  p^ := s;
  writeln(buf);
end.
