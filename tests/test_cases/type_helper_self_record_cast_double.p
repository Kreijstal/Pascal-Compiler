{$mode objfpc}
{$modeswitch typehelpers}
program type_helper_self_record_cast_double;

type
  TDoubleRec = packed record
    LowPart: LongWord;
    HighPart: LongWord;
  end;

  TDoubleHelper = type helper for Double
    function GetHigh: LongWord;
  end;

function TDoubleHelper.GetHigh: LongWord;
begin
  Result := TDoubleRec(Self).HighPart;
end;

var
  d: Double;
  h: LongWord;

begin
  d := 1.0;
  h := d.GetHigh;
  Writeln(h);
end.
