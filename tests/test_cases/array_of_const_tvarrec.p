{$mode objfpc}
program array_of_const_tvarrec;

uses
  SysUtils;

procedure Dump(const Args: array of const);
begin
  if Args[0].VType = vtInteger then
    writeln('int ', Args[0].VInteger)
  else
    writeln('bad');
end;

begin
  Dump([42]);
end.
