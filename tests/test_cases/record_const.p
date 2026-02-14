program test_record_const;
{$mode delphi}

type
  TMyRec = record
  const
    Bias = 42;
  public
    Value: Integer;
    function GetBiased: Integer;
  end;

function TMyRec.GetBiased: Integer;
begin
  Result := Value + Bias;
end;

var
  r: TMyRec;
begin
  r.Value := 10;
  WriteLn(r.GetBiased);
  WriteLn(TMyRec.Bias);
end.
