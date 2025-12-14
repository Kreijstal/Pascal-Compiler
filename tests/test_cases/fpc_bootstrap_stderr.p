{ Test: stderr standard file variable }
{ From FPC errors.pp - WriteLn(stderr, ...) is used for error output }
program fpc_bootstrap_stderr;
{$mode objfpc}

begin
  WriteLn(stderr, 'Error to stderr');
  WriteLn('Normal to stdout');
end.
