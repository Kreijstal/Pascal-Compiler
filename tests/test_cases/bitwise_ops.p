program BitwiseOpsTest;
begin
  writeln(8 shl 2);    { 32 }
  writeln(32 shr 2);   { 8 }
  writeln(10 xor 3);   { 9 }
  writeln(12 xor 7);   { 11 }
  writeln(5 shl 1);    { 10 }
  writeln(20 shr 1);   { 10 }
  writeln(305419896 rol 8);   { 878082066 }
  writeln(305419896 ror 12);  { 1736516421 }
  writeln((1 shl 31) rol 1);  { 1 }
  writeln((1 shl 31) ror 1);  { 1073741824 }
end.
