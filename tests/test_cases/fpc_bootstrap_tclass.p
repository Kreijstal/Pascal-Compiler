{ Test TClass type - built-in type for class references }
{ Required for FPC bootstrap objpas.pp which declares: ExceptionClass: TClass }
program fpc_bootstrap_tclass;

{$mode objfpc}

type
  TMyClass = class
  end;

var
  ClassRef: TClass;
begin
  ClassRef := TMyClass;
  if ClassRef = TMyClass then
    WriteLn('TClass works')
  else
    WriteLn('TClass problem');
end.
