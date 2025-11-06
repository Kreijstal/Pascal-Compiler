program LambdaTypeChecking;
{
  Test lambda/anonymous method type checking.
  This tests that the compiler correctly type-checks anonymous functions
  and procedures without attempting code generation.
}

type
  TIntFunc = reference to function(x: Integer): Integer;
  TIntProc = reference to procedure(x: Integer);
  TNoParamFunc = reference to function: Integer;
  TNoParamProc = reference to procedure;

var
  f1: TIntFunc;
  f2: TIntFunc;
  p1: TIntProc;
  p2: TIntProc;
  nf: TNoParamFunc;
  np: TNoParamProc;

begin
  { These assignments should pass type checking }
  f1 := function(x: Integer): Integer
  begin
    Result := x + 1
  end;
  
  f2 := function(y: Integer): Integer
  begin
    Result := y * 2
  end;
  
  p1 := procedure(x: Integer)
  begin
    WriteLn(x)
  end;
  
  p2 := procedure(val: Integer)
  begin
    WriteLn('Value: ', val)
  end;
  
  nf := function: Integer
  begin
    Result := 42
  end;
  
  np := procedure
  begin
    WriteLn('Hello')
  end;
  
  WriteLn('Type checking passed!');
end.
