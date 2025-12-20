program fpgetenv_overload;

{$mode objfpc}

function fpgetenv(name: PAnsiChar): PAnsiChar; overload;
begin
  fpgetenv := 'pchar';
end;

function fpgetenv(name: ShortString): PAnsiChar; overload;
begin
  fpgetenv := 'short';
end;

begin
  writeln(fpgetenv('PATH'));
end.
