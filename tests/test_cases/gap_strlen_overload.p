{$mode objfpc}
program gap_strlen_overload;

function strlen(p: PChar): SizeInt; overload;
begin
  if p = nil then
    Exit(0);
  Result := 1;
end;

function strlen(p: PWideChar): SizeInt; overload;
begin
  if p = nil then
    Exit(0);
  Result := 2;
end;

var
  s: PChar;

begin
  s := 'a';
  WriteLn(strlen(s));
end.
