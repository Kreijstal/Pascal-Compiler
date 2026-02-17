program tdd_sysutils_exception_handling;

{$mode objfpc}

uses SysUtils;

var
  cleaned: Boolean;
  s: AnsiString;
  v: Integer;

function SafeStrToInt(const S: AnsiString): Integer;
begin
  try
    Result := StrToInt(S);
  except
    on E: Exception do
    begin
      writeln('SafeStrToInt caught: ', E.Message);
      Result := -1;
    end;
  end;
end;

procedure TestNestedFinally;
var
  step: Integer;
begin
  step := 0;
  try
    step := 1;
    try
      step := 2;
      raise Exception.Create('inner error');
    finally
      step := 3;
      writeln('Inner finally: step=', step);
    end;
  except
    on E: Exception do
    begin
      writeln('Outer except: ', E.Message, ' step=', step);
    end;
  end;
end;

procedure TestExceptionCreateFmt;
var
  ex: Exception;
begin
  try
    raise Exception.CreateFmt('Error %d: %s', [42, 'bad input']);
  except
    on E: Exception do
      writeln('CreateFmt: ', E.Message);
  end;
end;

procedure TestMultipleHandlers;
begin
  try
    raise Exception.Create('generic error');
  except
    on E: Exception do
      writeln('Handler caught: ', E.Message);
  end;
end;

procedure TestFinallyCleanup;
var
  resource: AnsiString;
begin
  resource := 'allocated';
  try
    writeln('Resource: ', resource);
    raise Exception.Create('cleanup test');
  except
    on E: Exception do
      writeln('Caught: ', E.Message);
  end;
  { After except, resource should still be valid }
  writeln('After except resource: ', resource);
end;

procedure TestExceptionInFunction;
begin
  try
    v := SafeStrToInt('456');
    writeln('SafeStrToInt in try: ', v);
  except
    on E: Exception do
      writeln('Should not catch here');
  end;
end;

procedure TestRaisedAndReCaught;
var
  originalMsg: AnsiString;
begin
  originalMsg := 'original';
  try
    try
      raise Exception.Create(originalMsg);
    except
      on E: Exception do
      begin
        writeln('First catch: ', E.Message);
        raise;
      end;
    end;
  except
    on E: Exception do
      writeln('Second catch after re-raise: ', E.Message);
  end;
end;

function IntDivideWithExceptionHandling(a, b: Integer): Integer;
begin
  try
    if b = 0 then
      raise Exception.Create('Division by zero');
    Result := a div b;
  except
    on E: Exception do
    begin
      writeln('IntDivide caught: ', E.Message);
      Result := 0;
    end;
  end;
end;

procedure TestExceptionHierarchy;
begin
  try
    try
      raise Exception.Create('Base exception');
    except
      on E: Exception do
        writeln('Caught base Exception: ', E.Message);
    end;
  except
    on E: Exception do
      writeln('Should not reach');
  end;
end;

procedure TestFinallyExecutionOrder;
var
  callOrder: AnsiString;
begin
  callOrder := '';
  try
    try
      callOrder := callOrder + '1,';
      raise Exception.Create('test');
    finally
      callOrder := callOrder + '2,';
    end;
  except
    on E: Exception do
      callOrder := callOrder + '3';
  end;
  writeln('Finally order: ', callOrder);
end;

begin
  { Test 1: Basic Exception.Create }
  try
    raise Exception.Create('test message');
  except
    on E: Exception do
      writeln('Test1: ', E.Message);
  end;

  { Test 2: Exception.CreateFmt }
  TestExceptionCreateFmt;

  { Test 3: Nested try/finally inside try/except }
  TestNestedFinally;

  { Test 4: SafeStrToInt with valid input }
  v := SafeStrToInt('123');
  writeln('SafeStrToInt valid: ', v);

  { Test 5: SafeStrToInt with invalid input }
  v := SafeStrToInt('not_a_number');
  writeln('SafeStrToInt invalid: ', v);

  { Test 6: Multiple handlers }
  TestMultipleHandlers;

  { Test 7: Finally cleanup }
  TestFinallyCleanup;

  { Test 8: try/finally without exception }
  cleaned := false;
  try
    s := 'no error';
  finally
    cleaned := true;
  end;
  if cleaned then
    writeln('Finally no-error: OK')
  else
    writeln('Finally no-error: FAIL');

  { Test 9: Nested exceptions }
  try
    try
      raise Exception.Create('level 1');
    except
      on E: Exception do
      begin
        writeln('Inner: ', E.Message);
        try
          raise Exception.Create('level 2');
        except
          on E2: Exception do
            writeln('Nested: ', E2.Message);
        end;
      end;
    end;
  except
    on E: Exception do
      writeln('Should not reach here');
  end;

  { Test 10: Exception in function }
  TestExceptionInFunction;

  { Test 11: Raised and re-caught }
  TestRaisedAndReCaught;

  { Test 12: Integer division with exception handling }
  v := IntDivideWithExceptionHandling(10, 2);
  writeln('IntDivide 10/2: ', v);

  v := IntDivideWithExceptionHandling(10, 0);
  writeln('IntDivide 10/0: ', v);

  { Test 13: Exception hierarchy }
  TestExceptionHierarchy;

  { Test 14: Finally execution order }
  TestFinallyExecutionOrder;

  writeln('Done');
end.
