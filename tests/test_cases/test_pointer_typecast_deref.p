program test_pointer_typecast_deref;

{ Test case for pointer typecast and dereference bug fix
  This test verifies that complex pointer dereference chains with typecasts
  work correctly, specifically: PType(pointer_expr)^
  
  Bug: Previously, typecasts to pointer types didn't preserve the pointed-to
  type information, causing the dereference to default to LONGINT_TYPE instead
  of the correct record type.
  
  Fix: semcheck_typecast now looks up the target type and extracts what it
  points to, preserving record_type and pointer_subtype_id information. }

type
  TInAddr = record
    S_addr: Integer;
  end;
  PInAddr = ^TInAddr;

  THostEnt = record
    h_addr_list: Pointer;
  end;
  PHostEnt = ^THostEnt;

var
  Server: PHostEnt;
  Addr: TInAddr;
  AddrPtr: PInAddr;
  TestPassed: Boolean;

begin
  TestPassed := True;
  
  { Allocate test data }
  New(Server);
  New(AddrPtr);
  
  { Set up test data }
  AddrPtr^.S_addr := 12345;
  Server^.h_addr_list := AddrPtr;
  
  { Test 1: Using intermediate variable (should work) }
  WriteLn('Test 1: Intermediate variable assignment');
  AddrPtr := PInAddr(Server^.h_addr_list);
  Addr := AddrPtr^;
  
  if Addr.S_addr <> 12345 then
  begin
    WriteLn('FAILED: Test 1 - Expected 12345, got ', Addr.S_addr);
    TestPassed := False;
  end
  else
    WriteLn('PASSED: Test 1');
  
  { Test 2: Direct assignment with typecast and dereference in one expression }
  { This is the bug that was fixed - previously failed with:
    "type mismatch in assignment statement (lhs: record, rhs: longint)" }
  WriteLn('Test 2: Direct typecast and dereference');
  Addr.S_addr := 0; { Reset }
  Addr := PInAddr(Server^.h_addr_list)^;
  
  if Addr.S_addr <> 12345 then
  begin
    WriteLn('FAILED: Test 2 - Expected 12345, got ', Addr.S_addr);
    TestPassed := False;
  end
  else
    WriteLn('PASSED: Test 2');
  
  { Test 3: Verify field access after typecast dereference }
  WriteLn('Test 3: Field access after typecast dereference');
  if PInAddr(Server^.h_addr_list)^.S_addr <> 12345 then
  begin
    WriteLn('FAILED: Test 3 - Field access incorrect');
    TestPassed := False;
  end
  else
    WriteLn('PASSED: Test 3');
  
  { Cleanup }
  Dispose(AddrPtr);
  Dispose(Server);
  
  { Report final result }
  WriteLn;
  if TestPassed then
    WriteLn('All tests PASSED')
  else
    WriteLn('Some tests FAILED');
end.
