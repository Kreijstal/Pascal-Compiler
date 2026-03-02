{$mode objfpc}

program tdd_nested_record_result_alias;

type
  TOuter = class
  public
    type
      TEnum = record
        FCurrentPosition: PChar;
        FEndPosition: PChar;
      end;
  public
    function GetEnum: TEnum;
  end;

function TOuter.GetEnum: TEnum;
begin
  Result.FCurrentPosition := nil;
  Result.FEndPosition := nil;
end;

var
  O: TOuter;
  E: TOuter.TEnum;

begin
  O := TOuter.Create;
  E := O.GetEnum;
  if (E.FCurrentPosition = nil) and (E.FEndPosition = nil) then
    writeln('OK')
  else
    writeln('BAD');
end.
