program tdd_record_property_deref;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRec = record
  private
    function GetVal: Longint;
  public
    X: Longint;
    property Val: Longint read GetVal;
  end;
  PRec = ^TRec;

function TRec.GetVal: Longint;
begin
  Result := X;
end;

var
  R: TRec;
  P: PRec;

begin
  R.X := 7;
  P := @R;
  writeln(P^.Val);
end.
