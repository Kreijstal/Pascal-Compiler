program const_expr_operators;

const
  C1: LongInt = (5 shl 3) + 1;
  C2: LongInt = (64 shr 2) or 1;
  C3: LongInt = 15 and 6;
  C4: LongInt = 15 xor 6;
  C5: LongInt = not 0;
  C6: LongInt = not 1;
  C7: LongInt = 1 shl 0;
  C8: LongInt = 1 shl 30;

begin
  writeln(C1);
  writeln(C2);
  writeln(C3);
  writeln(C4);
  writeln(C5);
  writeln(C6);
  writeln(C7);
  writeln(C8);
end.
