program tdd_record_property_pointer_kind;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TKindRec = record
    Kind: Longint;
  end;
  PKindRec = ^TKindRec;
  TRec = record
  private
    function GetPropType: PKindRec;
  public
    property PropType: PKindRec read GetPropType;
  end;

var
  G: TKindRec;

function TRec.GetPropType: PKindRec;
begin
  Result := @G;
end;

var
  R: TRec;

begin
  G.Kind := 42;
  writeln(R.PropType^.Kind);
end.
