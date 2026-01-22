program tencoding_getansistring_ret;

{$mode objfpc}

uses sysutils;

function MakeBytes: TBytes;
begin
  Result := TEncoding.UTF8.GetBytes('Hi');
end;

var
  s: AnsiString;

begin
  s := TEncoding.UTF8.GetAnsiString(MakeBytes);
  writeln(s);
end.
