program class_helper_instance_method_repro;
{$mode objfpc}
type
  TBox = class
    Value: Integer;
  end;

  TBoxHelper = class helper for TBox
    function Twice: Integer;
  end;

function TBoxHelper.Twice: Integer;
begin
  Result := Value * 2;
end;

var
  b: TBox;
begin
  b := TBox.Create;
  b.Value := 9;
  WriteLn(b.Twice);
end.
