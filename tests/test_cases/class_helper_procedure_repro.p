program class_helper_procedure_repro;
{$mode objfpc}
type
  TBox = class
    Value: Integer;
  end;

  TBoxHelper = class helper for TBox
    procedure SetValueTwice(x: Integer);
  end;

procedure TBoxHelper.SetValueTwice(x: Integer);
begin
  Value := x * 2;
end;

var
  b: TBox;
begin
  b := TBox.Create;
  b.SetValueTwice(8);
  WriteLn(b.Value);
end.
