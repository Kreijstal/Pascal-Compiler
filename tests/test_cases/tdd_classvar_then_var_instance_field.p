program tdd_classvar_then_var_instance_field;

{$mode objfpc}

type
  TBox = class
    class var
      Shared: LongInt;
    var
      Value: LongInt;
  end;

var
  FirstBox: TBox;
  SecondBox: TBox;

begin
  FirstBox := TBox.Create;
  SecondBox := TBox.Create;
  FirstBox.Value := 7;
  SecondBox.Value := 11;
  WriteLn(FirstBox.Value);
  WriteLn(SecondBox.Value);
end.
