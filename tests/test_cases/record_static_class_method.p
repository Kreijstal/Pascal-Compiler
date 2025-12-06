{$mode objfpc}
{$modeswitch advancedrecords}
program record_static_class_method;
{ Test case for static class methods on advanced records }
{ This tests parsing of: class procedure ... static; declarations }
{ Full static method call syntax (TCounter.PrintHello) is TODO }

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
  { Static method called via mangled name with dummy instance }
  { TODO: Support TCounter.PrintHello; syntax for true static call }
  C.Value := 0;
  TCounter__PrintHello(C);
end.
