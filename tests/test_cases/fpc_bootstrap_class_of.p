{ Test: class of - metaclass type }
{ FPC RTL uses class references for class registration }
program fpc_bootstrap_class_of;
{$mode objfpc}

type
  TMyClass = class
    function GetName: string;
  end;
  
  TMyClassType = class of TMyClass;

function TMyClass.GetName: string;
begin
  Result := 'TMyClass';
end;

var
  ClassRef: TMyClassType;
  obj: TMyClass;
begin
  ClassRef := TMyClass;
  obj := ClassRef.Create;
  WriteLn(obj.GetName);
end.
