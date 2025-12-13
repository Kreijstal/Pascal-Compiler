{ Test for FPC bootstrap: TClass/class of type reference }
{ This type is used in many FPC RTL units including objpas.pp }
{$mode objfpc}
program fpc_bootstrap_tclass_type;

type
  { "class of" creates a class reference type }
  TMyClass = class of TObject;

var
  c: TMyClass;

begin
  { Test that class reference type is recognized }
  c := TObject;
  WriteLn('TClass type works');
end.
