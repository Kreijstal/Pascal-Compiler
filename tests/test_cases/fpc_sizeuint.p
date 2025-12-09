program FPCSizeUInt;
{
  Test SizeUInt type which is used throughout FPC RTL.
  It's an alias for a platform-dependent unsigned integer type.
}

var
  s: SizeUInt;
  i: integer;

begin
  s := 1000;
  WriteLn('SizeUInt value: ', s);
  WriteLn('SizeUInt size: ', SizeOf(SizeUInt));
  
  { Test arithmetic }
  s := s + 500;
  WriteLn('After adding 500: ', s);
  
  s := s * 2;
  WriteLn('After multiplying by 2: ', s);
  
  WriteLn('Test completed');
end.
