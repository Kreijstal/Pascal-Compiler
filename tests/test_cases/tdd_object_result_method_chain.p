{$mode objfpc}
program tdd_object_result_method_chain;

type
  TTemp = object
    Value: LongInt;
    procedure ResetIfTemp;
  end;

  THelper = object
    function MakeTemp: TTemp;
  end;

procedure TTemp.ResetIfTemp;
begin
  WriteLn('reset=', Value);
end;

function THelper.MakeTemp: TTemp;
begin
  Result.Value := 7;
end;

var
  H: THelper;
begin
  H.MakeTemp.ResetIfTemp;
end.
