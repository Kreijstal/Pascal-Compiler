{$mode objfpc}
program reg_overload_ptr_alias_equiv;

type
  PCharA = PAnsiChar; // alias

function Which(p: PAnsiChar): Integer; overload;
begin
  Which := 1;
end;

function Which(p: PCharA): Integer; overload;
begin
  Which := 2;
end;

var
  p: PAnsiChar;

begin
  p := PAnsiChar('abc');
  Writeln(Which(p));
end.
