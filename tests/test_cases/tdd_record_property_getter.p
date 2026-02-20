program tdd_record_property_getter;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRec = record
  private
    function GetValue: Integer;
  public
    property Value: Integer read GetValue;
  end;

function TRec.GetValue: Integer;
begin
  Result := 42;
end;

var
  Rec: TRec;

begin
  Writeln(Rec.Value);
end.
