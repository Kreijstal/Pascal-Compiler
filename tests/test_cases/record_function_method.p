{$mode objfpc}
{$modeswitch advancedrecords}
program record_function_method;
{ Test case for calling function methods on records }
{ This tests advanced record support - record types with methods }
{ KGPC should support calling both procedure and function methods on records }

type
  TCounter = record
    Value: Integer;
    function GetDouble: Integer;
  end;

function TCounter.GetDouble: Integer;
begin
  GetDouble := Value * 2;
end;

var
  C: TCounter;
begin
  C.Value := 10;
  writeln(C.GetDouble);
end.
