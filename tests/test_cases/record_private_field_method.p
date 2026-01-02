program record_private_field_method;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRec = record
  private
    FValue: Integer;
  public
    procedure SetValue(A: Integer);
    function GetValue: Integer;
  end;

procedure TRec.SetValue(A: Integer);
begin
  FValue := A;
end;

function TRec.GetValue: Integer;
begin
  Result := FValue;
end;

var
  R: TRec;

begin
  R.SetValue(5);
  writeln(R.GetValue);
end.
