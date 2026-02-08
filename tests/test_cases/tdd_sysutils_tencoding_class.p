program tdd_sysutils_tencoding_class;

uses SysUtils;

var
  enc: TEncoding;
  bytes: TBytes;
  s: AnsiString;
  i: Integer;
  tmpStr: String;
  defEnc: TEncoding;
  sysEnc: TEncoding;
begin
  { Test 1: Singleton access - UTF8/ANSI should return non-nil }
  enc := TEncoding.UTF8;
  if enc <> nil then writeln('Test 1 UTF8: OK') else writeln('Test 1 UTF8: NIL');
  
  enc := TEncoding.ANSI;
  if enc <> nil then writeln('Test 2 ANSI: OK') else writeln('Test 2 ANSI: NIL');

  defEnc := TEncoding.SystemEncoding;
  if defEnc <> nil then writeln('Test 3 SystemEncoding: OK') else writeln('Test 3 SystemEncoding: NIL');
  
  { Test 4: Singletons are cached - same instance returned }
  if TEncoding.UTF8 = TEncoding.UTF8 then
    writeln('Test 4 UTF8 singleton cache: OK')
  else
    writeln('Test 4 UTF8 singleton cache: FAIL');
  
  if TEncoding.ANSI = TEncoding.ANSI then
    writeln('Test 5 ANSI singleton cache: OK')
  else
    writeln('Test 5 ANSI singleton cache: FAIL');
  
  { Test 6: GetBytes basic - convert string to bytes }
  enc := TEncoding.UTF8;
  bytes := enc.GetBytes('Hello');
  writeln('Test 6 GetBytes len: ', Length(bytes));
  write('Test 6 GetBytes values: ');
  for i := 0 to Length(bytes) - 1 do
    write(bytes[i], ' ');
  writeln;
  
  { Test 7: GetString round-trip - convert bytes back to string }
  tmpStr := enc.GetString(bytes);
  writeln('Test 7 Round-trip result: ', tmpStr);
  if tmpStr = 'Hello' then
    writeln('Test 7 Round-trip: OK')
  else
    writeln('Test 7 Round-trip: FAIL');
  
  { Test 8: GetAnsiBytes with slicing }
  bytes := enc.GetAnsiBytes('Hello', 1, 3);
  writeln('Test 8 GetAnsiBytes slice len: ', Length(bytes));
  s := enc.GetAnsiString(bytes);
  writeln('Test 8 GetAnsiBytes slice result: ', s);
  
  { Test 9: GetAnsiBytes edge - empty string }
  bytes := enc.GetAnsiBytes('', 0, 0);
  writeln('Test 9 GetAnsiBytes empty len: ', Length(bytes));
  if Length(bytes) = 0 then
    writeln('Test 9 GetAnsiBytes empty: OK')
  else
    writeln('Test 9 GetAnsiBytes empty: FAIL');
  
  { Test 10: GetAnsiString with index/count }
  bytes := enc.GetBytes('ABCDEF');
  s := enc.GetAnsiString(bytes, 2, 3);
  writeln('Test 10 GetAnsiString slice result: ', s);
  writeln('Test 10 GetAnsiString slice len: ', Length(s));
  
  { Test 11: GetString with index/count }
  tmpStr := enc.GetString(bytes, 0, 2);
  writeln('Test 11 GetString slice result: ', tmpStr);
  writeln('Test 11 GetString slice len: ', Length(tmpStr));
  
  { Test 12: Class property SystemEncoding }
  enc := TEncoding.SystemEncoding;
  if enc <> nil then
    writeln('Test 12 SystemEncoding: OK')
  else
    writeln('Test 12 SystemEncoding: NIL');
  
  { Test 13: Single char round-trip }
  enc := TEncoding.UTF8;
  bytes := enc.GetBytes('X');
  writeln('Test 13 Single char bytes len: ', Length(bytes));
  tmpStr := enc.GetString(bytes);
  writeln('Test 13 Single char result: ', tmpStr);
  if tmpStr = 'X' then
    writeln('Test 13 Single char: OK')
  else
    writeln('Test 13 Single char: FAIL');

  { Test 14: Unicode characters }
  bytes := enc.GetBytes('Café');
  writeln('Test 14 Unicode bytes len: ', Length(bytes));
  tmpStr := enc.GetString(bytes);
  writeln('Test 14 Unicode result: ', tmpStr);
  if tmpStr = 'Café' then
    writeln('Test 14 Unicode: OK')
  else
    writeln('Test 14 Unicode: FAIL');

  { Test 15: Multiple round-trips }
  enc := TEncoding.ANSI;
  bytes := enc.GetBytes('Test');
  tmpStr := enc.GetString(bytes);
  bytes := enc.GetBytes(tmpStr);
  tmpStr := enc.GetString(bytes);
  writeln('Test 15 Multiple round-trip result: ', tmpStr);
  if tmpStr = 'Test' then
    writeln('Test 15 Multiple round-trip: OK')
  else
    writeln('Test 15 Multiple round-trip: FAIL');

  { Test 16: Large string }
  tmpStr := '';
  for i := 0 to 99 do
    tmpStr := tmpStr + 'A';
  enc := TEncoding.UTF8;
  bytes := enc.GetBytes(tmpStr);
  writeln('Test 16 Large string bytes len: ', Length(bytes));
  tmpStr := enc.GetString(bytes);
  writeln('Test 16 Large string len: ', Length(tmpStr));
  if Length(tmpStr) = 100 then
    writeln('Test 16 Large string: OK')
  else
    writeln('Test 16 Large string: FAIL');

  { Test 17: Verify SystemEncoding is non-nil }
  sysEnc := TEncoding.SystemEncoding;
  if sysEnc <> nil then
    writeln('Test 17 SystemEncoding non-nil: OK')
  else
    writeln('Test 17 SystemEncoding non-nil: FAIL');

  { Test 18: Verify different encodings produce different results for unicode }
  enc := TEncoding.UTF8;
  bytes := enc.GetBytes('é');
  writeln('Test 18 UTF8 bytes for é: ', Length(bytes));
  
  enc := TEncoding.ANSI;
  bytes := enc.GetBytes('é');
  writeln('Test 18 ANSI bytes for é: ', Length(bytes));

  writeln('=== All tests completed ===');
end.
