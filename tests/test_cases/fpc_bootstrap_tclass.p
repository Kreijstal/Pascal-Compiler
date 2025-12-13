{ Test TClass type - built-in type for class references }
{ Required for FPC bootstrap objpas.pp which declares: ExceptionClass: TClass }
program fpc_bootstrap_tclass;

{$mode objfpc}

var
  ClassRef: TClass;
begin
  ClassRef := nil;
  WriteLn('TClass type works');
end.
