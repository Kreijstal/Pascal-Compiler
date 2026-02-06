{$mode objfpc}
{$H+}
program reg_overload_default_string_mode;

function Which(const S: AnsiString): Integer; overload;
begin
  Which := 1;
end;

function Which(const S: UnicodeString): Integer; overload;
begin
  Which := 2;
end;

var
  s: string;

begin
  s := 'abc';
  Writeln(Which(s));
end.
