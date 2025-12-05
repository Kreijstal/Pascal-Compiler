{$mode objfpc}
{$modeswitch advancedrecords}
program record_static_class_method;
{ Test case for static class methods on advanced records }
{ This tests: class procedure ... static; on record types }
{ FPC supports this feature with advanced records mode switch }

type
  TCounter = record
    Value: Integer;
    class procedure PrintHello; static;
  end;

class procedure TCounter.PrintHello;
begin
  writeln('Hello from static method');
end;

var
  C: TCounter;
begin
  { Instance method call works via method call syntax }
  C.Value := 0;
  TCounter__PrintHello(C);
end.
