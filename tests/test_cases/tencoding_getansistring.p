program tencoding_getansistring;

{$mode objfpc}

uses sysutils;

var
  bytes: TBytes;
  s: AnsiString;

begin
  bytes := TEncoding.UTF8.GetBytes('Hi');
  s := TEncoding.UTF8.GetAnsiString(bytes);
  writeln(s);
end.
