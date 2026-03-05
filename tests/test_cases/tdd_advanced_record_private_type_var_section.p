program tdd_advanced_record_private_type_var_section;
{$mode delphi}

type
  TRec = record
  private type
    TNested = record
      V: Integer;
    end;
  var
    FX: Integer;
  public
    procedure SetX(A: Integer);
    function ValueFromNested: Integer;
  end;

procedure TRec.SetX(A: Integer);
begin
  FX := A;
end;

function TRec.ValueFromNested: Integer;
var
  N: TNested;
begin
  N.V := FX;
  Result := N.V;
end;

var
  R: TRec;
begin
  R.SetX(42);
  Writeln(R.ValueFromNested);
end.
