program tdd_types_equals_default_method;
{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRec = record
    X: Integer;
    function EqualsTo(const R: TRec; const Epsilon: Single = 0): Boolean;
  end;

function TRec.EqualsTo(const R: TRec; const Epsilon: Single): Boolean;
begin
  Result := (X = R.X) and (Epsilon = 0);
end;

var
  A, B: TRec;
  Ok: Boolean;
begin
  A.X := 42;
  B.X := 42;
  Ok := A.EqualsTo(B);
  if Ok then
    WriteLn('PASS')
  else
    WriteLn('FAIL');
end.
