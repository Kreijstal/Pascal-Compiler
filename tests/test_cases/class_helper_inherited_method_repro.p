program class_helper_inherited_method_repro;
{$mode objfpc}
type
  TBase = class
    Value: Integer;
  end;

  TChild = class(TBase)
  end;

  TBaseHelper = class helper for TBase
    function Twice: Integer;
  end;

function TBaseHelper.Twice: Integer;
begin
  Result := Value * 2;
end;

var
  c: TChild;
begin
  c := TChild.Create;
  c.Value := 6;
  WriteLn(c.Twice);
end.
