program fpc_regression_findclose;
{$mode objfpc}

uses
  sysutils;

var
  SR: TSearchRec;
  ResultCode: Longint;

begin
  ResultCode := FindFirst('.', faAnyFile, SR);
  if ResultCode = 0 then
    FindClose(SR);
  Writeln('findclose ok');
end.
