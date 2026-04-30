program tdd_virtual_class_function_create_name;
{$mode objfpc}

type
  TDef = class
    Value: Integer;
  end;

  TNodeUtils = class
    class function CreateThing: TDef; virtual;
  end;

  TNodeUtilsClass = class of TNodeUtils;

class function TNodeUtils.CreateThing: TDef;
begin
  Result := TDef.Create;
  Result.Value := 42;
end;

var
  C: TNodeUtilsClass = TNodeUtils;
  D: TDef;

begin
  D := C.CreateThing;
  Writeln(D.Value);
end.
