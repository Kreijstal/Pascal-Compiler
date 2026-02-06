program fpc_regression_getansibytes;
{$mode objfpc}

uses
  sysutils;

var
  Bytes: TBytes;
  AnsiBytes: TBytes;

begin
  Bytes := TEncoding.UTF8.GetBytes('Hello');
  AnsiBytes := TEncoding.UTF8.GetAnsiBytes('Hi');
  Writeln(Length(Bytes), ':', Length(AnsiBytes));
end.
