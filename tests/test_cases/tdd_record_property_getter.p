program tdd_record_property_getter;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRec = record
  private
    function GetProp(Index: Word): Longint;
  public
    property Prop[Index: Word]: Longint read GetProp;
  end;

function TRec.GetProp(Index: Word): Longint;
begin
  Result := Index + 1;
end;

var
  R: TRec;

begin
  writeln(R.Prop[2]);
end.
