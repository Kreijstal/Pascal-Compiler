{$mode objfpc}
program regression_strlen_addr;

uses
  sysutils;

var
  buf: array[0..3] of char;

begin
  buf[0] := 'a';
  buf[1] := #0;
  WriteLn(strlen(@buf[0]));
end.
