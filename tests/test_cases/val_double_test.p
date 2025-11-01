program val_double_test;
var
  s: string;
  value: Double;
  code: Integer;
  number: Integer;
begin
  s := '3.1415';
  value := 0.0;
  code := -1;
  number := -1;
  Val(s, value, code);
  if (code = 0) and (value > 3.0) then
    WriteLn('Value: ', value:0:4, ' Code: ', code);

  s := '42';
  Val(s, number, code);
  WriteLn('Integer: ', number, ' Code: ', code);

  s := '7Z';
  number := -5;
  Val(s, number, code);
  WriteLn('Invalid: ', number, ' Code: ', code);
end.
