program tencoding_basic;

{$mode objfpc}

uses sysutils;

var
  bytes: TBytes;
  s: AnsiString;

begin
  bytes := TEncoding.UTF8.GetBytes('Hello');
  s := TEncoding.UTF8.GetString(bytes);
  writeln(s);
end.
