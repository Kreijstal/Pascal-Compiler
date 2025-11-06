program LambdaTypeErrors;
{
  Test lambda/anonymous method type error detection.
  These should all fail semantic checking with appropriate error messages.
}

type
  TIntFunc = reference to function(x: Integer): Integer;
  TRealFunc = reference to function(x: Real): Real;
  TIntProc = reference to procedure(x: Integer);
  TRealProc = reference to procedure(x: Real);

var
  f_int: TIntFunc;
  f_real: TRealFunc;
  p_int: TIntProc;
  p_real: TRealProc;

begin
  { This should fail: wrong parameter type }
  f_int := function(x: Real): Integer
  begin
    Result := Round(x)
  end;
  
  { This should fail: wrong return type }
  f_int := function(x: Integer): Real
  begin
    Result := x * 1.5
  end;
  
  { This should fail: function assigned to procedure variable }
  p_int := function(x: Integer): Integer
  begin
    Result := x + 1
  end;
  
  { This should fail: procedure assigned to function variable }
  f_int := procedure(x: Integer)
  begin
    WriteLn(x)
  end;
  
  WriteLn('Should not reach here');
end.
