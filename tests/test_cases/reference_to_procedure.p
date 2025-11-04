program ReferenceToProc;

type
  TVoidProc = reference to procedure(x: Integer);

procedure PrintValue(x: Integer);
begin
  WriteLn('Value: ', x)
end;

var
  P: TVoidProc;

begin
  { Reference to procedure types work with named procedures }
  P := PrintValue;
  P(42);
  
  WriteLn('Reference to procedure test passed');
end.
