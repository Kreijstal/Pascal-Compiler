{ TDD test: Reproduces the exact sysutils.pp pattern that fails }
unit fpc_bootstrap_sysutils_pattern;

interface

uses
  baseunix;

implementation

uses
  objpas;

{$i includes/sysutils_minimal.inc}

initialization
  InitExceptions;
finalization
  DoneExceptions;
end.
