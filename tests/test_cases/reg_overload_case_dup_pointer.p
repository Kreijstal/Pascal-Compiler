{$mode objfpc}
program reg_overload_case_dup_pointer;

function StrPos(p: PAnsiChar): PAnsiChar; overload;
begin
  StrPos := p;
end;

function strpos(p: PAnsiChar): PAnsiChar; overload;
begin
  strpos := p;
end;

var
  p: PAnsiChar;

begin
  p := nil;
  if StrPos(p) = nil then
    Writeln('ok');
end.
