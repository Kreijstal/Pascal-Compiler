program tdd_nested_method_param_named_nostackframe;
{$mode objfpc}

type
  TObj = class
    Value: LongInt;
    procedure DoIt(nostackframe: Boolean);
  end;

procedure TObj.DoIt(nostackframe: Boolean);
  procedure Inner;
  begin
    if nostackframe then
      Value := 1;
  end;
begin
  Inner;
end;

var
  o: TObj;
begin
  o := TObj.Create;
  o.Value := 0;
  o.DoIt(True);
  writeln(o.Value);
end.
