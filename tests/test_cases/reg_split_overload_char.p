{$mode objfpc}
{$H+}
program reg_split_overload_char;

function Which(const A: array of AnsiChar): Integer; overload;
begin
  Which := 1;
end;

function Which(const A: array of AnsiString): Integer; overload;
begin
  Which := 2;
end;

function Call(const A: array of AnsiChar): Integer;
begin
  Result := Which(A);
end;

begin
  Writeln(Call(['a','b']));
end.
