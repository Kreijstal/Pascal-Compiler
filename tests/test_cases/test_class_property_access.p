{$mode objfpc}
program test_class_property_access;

{ Test case for class property access
  This test verifies that properties defined in classes can be accessed
  correctly, including read-only properties backed by private fields.
  
  Issue: Exception.Message property is not accessible in except blocks
  despite being properly defined in the Exception class. }

type
  TMyException = class
  private
    FMessage: string;
    FCode: Integer;
  public
    constructor Create(const Msg: string; Code: Integer);
    property Message: string read FMessage;
    property ErrorCode: Integer read FCode write FCode;
  end;

constructor TMyException.Create(const Msg: string; Code: Integer);
begin
  FMessage := Msg;
  FCode := Code;
end;

var
  Ex: TMyException;
  TestPassed: Boolean;

begin
  TestPassed := True;
  
  { Test 1: Read-only property access }
  WriteLn('Test 1: Read-only property (Message)');
  Ex := TMyException.Create('Test error message', 42);
  
  if Ex.Message <> 'Test error message' then
  begin
    WriteLn('FAILED: Expected "Test error message", got "', Ex.Message, '"');
    TestPassed := False;
  end
  else
    WriteLn('PASSED: Test 1');
  
  { Test 2: Read-write property access (read) }
  WriteLn('Test 2: Read-write property read (ErrorCode)');
  if Ex.ErrorCode <> 42 then
  begin
    WriteLn('FAILED: Expected 42, got ', Ex.ErrorCode);
    TestPassed := False;
  end
  else
    WriteLn('PASSED: Test 2');
  
  { Test 3: Read-write property access (write) }
  WriteLn('Test 3: Read-write property write (ErrorCode)');
  Ex.ErrorCode := 99;
  if Ex.ErrorCode <> 99 then
  begin
    WriteLn('FAILED: Expected 99, got ', Ex.ErrorCode);
    TestPassed := False;
  end
  else
    WriteLn('PASSED: Test 3');
  
  { Test 4: Property access in exception handler }
  WriteLn('Test 4: Property access in except block');
  try
    raise Ex;
  except
    on E: TMyException do
    begin
      if E.Message <> 'Test error message' then
      begin
        WriteLn('FAILED: Expected "Test error message" in except, got "', E.Message, '"');
        TestPassed := False;
      end
      else
        WriteLn('PASSED: Test 4');
    end;
  end;
  
  { Report final result }
  WriteLn;
  if TestPassed then
    WriteLn('All tests PASSED')
  else
    WriteLn('Some tests FAILED');
end.
