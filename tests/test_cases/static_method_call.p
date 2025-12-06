{$mode objfpc}
{$modeswitch advancedrecords}
program test_static_method_call;

{ Test static method calls using TRecordType.StaticMethod syntax }

type
  TCounter = record
    class function GetDefaultValue: Integer; static;
    class procedure PrintMessage; static;
  end;

class function TCounter.GetDefaultValue: Integer;
begin
  GetDefaultValue := 42;
end;

class procedure TCounter.PrintMessage;
begin
  writeln('Hello from static method');
end;

var
  x: Integer;
begin
  { Test static method call with function return value }
  x := TCounter.GetDefaultValue;
  writeln('Default value: ', x);
  
  { Test static procedure call }
  TCounter.PrintMessage;
  
  { Test direct use in expression }
  writeln('Double: ', TCounter.GetDefaultValue * 2);
end.
