program SimpleNonLocal;

const
  Size = 5;

var
  GlobalArray: array[0..Size] of integer;

procedure TestAccess;
begin
  GlobalArray[0] := 42;
  WriteLn('Set value to 42');
end;

begin
  GlobalArray[0] := 0;
  WriteLn('Initialized to 0');
  
  TestAccess;
  
  if GlobalArray[0] = 42 then
    WriteLn('Test passed')
  else
    WriteLn('Test FAILED');
    
  WriteLn('Done');
end.
