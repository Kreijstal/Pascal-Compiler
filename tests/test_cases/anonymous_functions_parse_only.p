program AnonymousFunctionsTest;
{$ifdef FPC}
  {$mode objfpc}{$H+}{$J-}
  {$modeswitch functionreferences}
  {$modeswitch anonymousfunctions}
{$endif}

type
  TIntMapFunc = reference to function(const Index, Item: Integer): Integer;
  TIntMapProc = reference to procedure(const Index, Item: Integer);
  
var
  F: TIntMapFunc;
  P: TIntMapProc;
  Result: Integer;

begin
  { These anonymous function definitions parse correctly }
  
  { Anonymous function with parameters and return type }
  F := function(const Index, Item: Integer): Integer
    begin
      Result := Item + 1
    end;
  
  { Anonymous procedure with parameters }
  P := procedure(const Index, Item: Integer)
    begin
      WriteLn('Index: ', Index, ', Item: ', Item)
    end;
  
  WriteLn('Anonymous functions parsed successfully');
  WriteLn('Note: Code generation for anonymous functions is not yet implemented');
end.
