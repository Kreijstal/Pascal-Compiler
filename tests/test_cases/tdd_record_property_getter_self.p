program tdd_record_property_getter_self;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRec = record
  private
    function GetProp(Index: Word): Longint;
  public
    property Prop[Index: Word]: Longint read GetProp;
    function Tail: Longint;
  end;

function TRec.GetProp(Index: Word): Longint;
begin
  Result := Index + 1;
end;

function TRec.Tail: Longint;
begin
  Result := Prop[2];
end;

var
  R: TRec;

begin
  writeln(R.Tail);
end.
