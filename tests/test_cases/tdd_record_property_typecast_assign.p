program tdd_record_property_typecast_assign;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRec = packed record
  private
    function GetExp: LongInt;
    procedure SetExp(v: LongInt);
  public
    property Exp: LongInt read GetExp write SetExp;
    case byte of
      0: (Data: QWord);
      1: (Value: Double);
  end;

function TRec.GetExp: LongInt;
begin
  Result := LongInt(Data and $7FFFFFFF);
end;

procedure TRec.SetExp(v: LongInt);
begin
  Data := QWord(v);
end;

var
  d: Double;

begin
  d := 0.0;
  TRec(d).Exp := 5;
  writeln(TRec(d).Exp);
end.
