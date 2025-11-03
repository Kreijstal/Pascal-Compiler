program ProcedureCallbackDemo;

type
  TCallback = procedure(value: Integer);

// A simple procedure that prints the value it receives.
procedure PrintValue(value: Integer);
begin
  WriteLn('Callback invoked with: ', value);
end;

// A different procedure that also matches the TCallback signature.
procedure PrintDouble(value: Integer);
begin
  WriteLn('Double callback with: ', value * 2);
end;

// This procedure takes a callback and a value, and calls the callback twice.
procedure PerformTwice(cb: TCallback; value: Integer);
begin
  cb(value);
  cb(value + 1);
end;

// This procedure just calls the callback once.
procedure Invoke(cb: TCallback; value: Integer);
begin
  cb(value);
end;

var
  handler: TCallback;

begin
  // First, test with the PrintValue callback.
  handler := PrintValue;
  PerformTwice(handler, 3); // Expected output: 3, then 4

  // Now, test with the PrintDouble callback.
  handler := PrintDouble;
  Invoke(handler, 5);       // Expected output: 10

  // Test passing a procedure directly without assigning to a variable first.
  PerformTwice(PrintValue, 7); // Expected output: 7, then 8
end.
