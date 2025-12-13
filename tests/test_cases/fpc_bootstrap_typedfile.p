{ Test TypedFile type - FPC built-in type alias for generic typed files }
{ Required for FPC bootstrap objpas.pp which declares: }
{ Procedure AssignFile(out f:TypedFile;p:PAnsiChar); }
program fpc_bootstrap_typedfile;

{$mode objfpc}

var
  f: TypedFile;
begin
  WriteLn('TypedFile type supported');
end.
