unit AdvancedRecordPrivateTypeVarSection;
{$mode delphi}

interface

type
  TOuter = record
  private type
    TNested = record
      A: Integer;
    end;
    function BuildValue: Integer;
  var
    FX: Integer;
  public
    procedure SetX(AValue: Integer);
  end;

implementation

function TOuter.BuildValue: Integer;
var
  N: TNested;
begin
  N.A := FX;
  Result := N.A;
end;

procedure TOuter.SetX(AValue: Integer);
begin
  FX := AValue;
end;

end.
