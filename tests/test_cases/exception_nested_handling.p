program exception_nested_handling;

{ Unit test for nested exception handling using integer-based exceptions }

var
  TestsPassed: Integer;
  TestsFailed: Integer;

procedure TestResult(const TestName: string; Passed: Boolean);
begin
  if Passed then
  begin
    writeln('[PASS] ', TestName);
    TestsPassed := TestsPassed + 1;
  end
  else
  begin
    writeln('[FAIL] ', TestName);
    TestsFailed := TestsFailed + 1;
  end;
end;

{ Test 1: Simple try-finally }
procedure Test_SimpleTryFinally;
var
  FinallyExecuted: Boolean;
begin
  FinallyExecuted := False;
  try
    writeln('  In try block');
  finally
    writeln('  In finally block');
    FinallyExecuted := True;
  end;
  TestResult('Simple try-finally', FinallyExecuted);
end;

{ Test 2: Nested try-finally }
procedure Test_NestedTryFinally;
var
  OuterFinallyExecuted: Boolean;
  InnerFinallyExecuted: Boolean;
begin
  OuterFinallyExecuted := False;
  InnerFinallyExecuted := False;
  try
    writeln('  Outer try');
    try
      writeln('  Inner try');
    finally
      writeln('  Inner finally');
      InnerFinallyExecuted := True;
    end;
  finally
    writeln('  Outer finally');
    OuterFinallyExecuted := True;
  end;
  TestResult('Nested try-finally', OuterFinallyExecuted and InnerFinallyExecuted);
end;

{ Test 3: Simple try-except with integer exception }
procedure Test_SimpleTryExcept;
var
  ExceptionCaught: Boolean;
begin
  ExceptionCaught := False;
  try
    writeln('  Raising exception 42');
    raise 42;
  except
    writeln('  Caught exception');
    ExceptionCaught := True;
  end;
  TestResult('Simple try-except', ExceptionCaught);
end;

{ Test 4: Try-except within try-finally }
procedure Test_ExceptWithinFinally;
var
  ExceptionCaught: Boolean;
  FinallyExecuted: Boolean;
begin
  ExceptionCaught := False;
  FinallyExecuted := False;
  try
    try
      writeln('  Inner try');
      raise 100;
    except
      writeln('  Caught in inner except');
      ExceptionCaught := True;
    end;
  finally
    writeln('  Outer finally');
    FinallyExecuted := True;
  end;
  TestResult('Try-except within try-finally', ExceptionCaught and FinallyExecuted);
end;

{ Test 5: Multiple nested exception handlers }
procedure Test_MultipleNestedExcept;
var
  OuterCaught: Boolean;
  InnerCaught: Boolean;
begin
  OuterCaught := False;
  InnerCaught := False;
  try
    writeln('  Outer try');
    try
      writeln('  Inner try');
      raise 200;
    except
      writeln('  Inner except');
      InnerCaught := True;
    end;
    writeln('  After inner except');
  except
    writeln('  Outer except');
    OuterCaught := True;
  end;
  TestResult('Multiple nested except', InnerCaught and not OuterCaught);
end;

{ Test 6: Exception propagation through finally }
procedure Test_ExceptionPropagation;
var
  FinallyExecuted: Boolean;
  ExceptionCaught: Boolean;
begin
  FinallyExecuted := False;
  ExceptionCaught := False;
  try
    try
      writeln('  Inner try');
      raise 300;
    finally
      writeln('  Finally block (should execute)');
      FinallyExecuted := True;
    end;
  except
    writeln('  Outer except');
    ExceptionCaught := True;
  end;
  TestResult('Exception propagation through finally', FinallyExecuted and ExceptionCaught);
end;

{ Test 7: Complex nesting - the original unix_http_request pattern }
procedure Test_ComplexNesting;
var
  InnerExceptExecuted: Boolean;
  OuterFinallyExecuted: Boolean;
begin
  InnerExceptExecuted := False;
  OuterFinallyExecuted := False;
  try
    try
      writeln('  Inner try');
      raise 400;
    except
      writeln('  Inner except');
      InnerExceptExecuted := True;
    end;
  finally
    writeln('  Outer finally');
    OuterFinallyExecuted := True;
  end;
  TestResult('Complex nesting (unix_http_request pattern)', InnerExceptExecuted and OuterFinallyExecuted);
end;

{ Test 8: Exception re-raise }
procedure Test_Reraise;
var
  InnerCaught: Boolean;
  OuterCaught: Boolean;
begin
  InnerCaught := False;
  OuterCaught := False;
  try
    try
      writeln('  Raising exception');
      raise 500;
    except
      writeln('  Inner except - re-raising');
      InnerCaught := True;
      raise;
    end;
  except
    writeln('  Outer except');
    OuterCaught := True;
  end;
  TestResult('Exception re-raise', InnerCaught and OuterCaught);
end;

begin
  TestsPassed := 0;
  TestsFailed := 0;
  
  writeln('=== Exception Handling Unit Tests ===');
  writeln;
  
  writeln('Test 1: Simple try-finally');
  Test_SimpleTryFinally;
  writeln;
  
  writeln('Test 2: Nested try-finally');
  Test_NestedTryFinally;
  writeln;
  
  writeln('Test 3: Simple try-except');
  Test_SimpleTryExcept;
  writeln;
  
  writeln('Test 4: Try-except within try-finally');
  Test_ExceptWithinFinally;
  writeln;
  
  writeln('Test 5: Multiple nested except');
  Test_MultipleNestedExcept;
  writeln;
  
  writeln('Test 6: Exception propagation through finally');
  Test_ExceptionPropagation;
  writeln;
  
  writeln('Test 7: Complex nesting (unix_http_request pattern)');
  Test_ComplexNesting;
  writeln;
  
  writeln('Test 8: Exception re-raise');
  Test_Reraise;
  writeln;
  
  writeln('=== Test Results ===');
  writeln('Passed: ', TestsPassed);
  writeln('Failed: ', TestsFailed);
  
  if TestsFailed = 0 then
    writeln('All tests passed!')
  else
    writeln('Some tests failed!');
end.
