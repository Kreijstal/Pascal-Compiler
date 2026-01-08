program fpc_bootstrap_gap_exception_createfmt;

{$mode objfpc}

uses SysUtils;

var
  E: Exception;

begin
  E := Exception.CreateFmt('val=%d', [5]);
  writeln(E.Message);
end.
