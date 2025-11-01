program CaseCharRange;
var
  Input: string;
  Position: Integer;
  Value: Integer;
begin
  Input := '123';
  Position := 1;
  case Input[Position] of
    '-', '0'..'9': Value := 1;
  else
    Value := 0;
  end;
  WriteLn('Result: ', Value);
end.
