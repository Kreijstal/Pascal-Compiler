{ Test: Result assigned via out parameter }
{ BUG: Result assigned by out param not recognized as return value }
{$mode objfpc}
program result_out_param;

function TryParse(const S: string; out Value: Integer): Boolean;
begin
  Value := 42;
  Result := True;
end;

function Parse(const S: string): Integer;
begin
  if not TryParse(S, Result) then
    WriteLn('Error');
end;

begin
  WriteLn(Parse('x'));
end.
