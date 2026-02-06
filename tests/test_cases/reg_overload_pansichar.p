{$mode objfpc}
{$H+}
program reg_overload_pansichar;

type
  PAnsiChar = ^AnsiChar;
  PWideChar = ^WideChar;

function Which(p: PAnsiChar): Integer; overload;
begin
  Which := 1;
end;

function Which(p: PWideChar): Integer; overload;
begin
  Which := 2;
end;

var
  p: PAnsiChar;

begin
  p := PAnsiChar('abc');
  Writeln(Which(p));
end.
