program test_dereference_minimal;

type
  PRec = ^TRec;
  TRec = record
    data: integer;
  end;

var
  p: PRec;

begin
  New(p);

  // 2. Test dereferencing for WRITE access.
  // The expression 'p^' resolves to the TRec on the heap.
  // We then access its 'data' field and assign a value.
  p^.data := 42;

  // 3. Test dereferencing for READ access.
  // We access the same field again to print its value.
  // If this prints "42", the test is successful.
  writeln(p^.data);
end.
