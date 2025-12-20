program const_expr_typecasts;

const
  C1: Cardinal = Cardinal(not Cardinal(0));
  C2: Int64 = Int64(Cardinal($FFFFFFFF));
  C3: QWord = QWord(1) shl 32;
  C4: LongInt = LongInt(Cardinal($FFFFFFFF));
  C5: Cardinal = Cardinal(Int64(-1));
  C6: Int64 = Int64(High(Cardinal));

begin
  writeln(C1);
  writeln(C2);
  writeln(C3);
  writeln(C4);
  writeln(C5);
  writeln(C6);
end.
