{ Test TypedFile type - FPC built-in type alias for generic typed files }
{ Required for FPC bootstrap objpas.pp which declares: }
{ Procedure AssignFile(out f:TypedFile;p:PAnsiChar); }
program fpc_bootstrap_typedfile;

{$mode objfpc}

procedure TestTypedFile(out f: TypedFile; const name: string);
begin
  Assign(f, name);
end;

begin
  WriteLn('TypedFile type supported');
end.
