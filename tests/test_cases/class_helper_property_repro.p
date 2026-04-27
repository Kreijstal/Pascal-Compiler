program class_helper_property_repro;
{$mode objfpc}
type
  TBox = class
    Value: Integer;
  end;

  TBoxHelper = class helper for TBox
    function GetTwice: Integer;
    property Twice: Integer read GetTwice;
  end;

function TBoxHelper.GetTwice: Integer;
begin
  Result := Value * 2;
end;

var
  b: TBox;
begin
  b := TBox.Create;
  b.Value := 11;
  WriteLn(b.Twice);
end.
