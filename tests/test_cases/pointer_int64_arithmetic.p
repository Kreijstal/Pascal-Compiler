{ Test for pointer arithmetic with Int64/SizeInt types }
{ This is a TDD test for FPC sysutils compatibility }
program pointer_int64_arithmetic;

type
  PAnsiChar = ^AnsiChar;
  SizeInt = Int64;

var
  S: AnsiString;
  P: PAnsiChar;
  Offset: SizeInt;
  Result: PAnsiChar;
begin
  S := 'Hello World';
  P := @S[1];
  Offset := 6;
  
  { pointer + int64 }
  Result := P + Offset;
  WriteLn(Result^);
  
  { pointer - int64 }
  Result := Result - Offset;
  WriteLn(Result^);
end.
