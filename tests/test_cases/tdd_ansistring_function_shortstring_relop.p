program TddAnsiStringFunctionShortStringRelop;
{$H+}

type
  PShortString = ^ShortString;

function AsAnsi(const S: AnsiString): AnsiString;
begin
  Result := S;
end;

var
  P: PShortString;

begin
  GetMem(P, Length('SYSTEM') + 1);
  P^ := 'SYSTEM';
  if AsAnsi('SYSTEM') <> P^ then
    Writeln('bad')
  else
    Writeln('ok');
  FreeMem(P);
end.
