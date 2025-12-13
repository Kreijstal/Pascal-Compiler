{ FPC Bootstrap Gap: shl operator in const expressions
  FPC uses patterns like:
    ln2bitsinword = 6;
    ln2bitmask = 1 shl ln2bitsinword - 1;
  This requires the const expression evaluator to support shl (shift left).
}
program fpc_shl_const;

const
  SHIFT_AMOUNT = 4;
  SHIFTED = 1 shl SHIFT_AMOUNT;  { Should be 16 }
  MASK = 1 shl 6 - 1;            { Should be 63 (64 - 1) }

begin
  writeln('1 shl 4 = ', SHIFTED);
  writeln('1 shl 6 - 1 = ', MASK);
end.
