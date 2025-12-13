{ FPC Bootstrap Gap: SizeOf() on user-defined types in const expressions
  FPC uses patterns like:
    type culong = qword;
    const BITSINWORD = 8*sizeof(culong);
  This requires the const expression evaluator to look up user-defined
  types and determine their size at compile time.
}
program fpc_sizeof_type_const;

type
  MyLong = LongWord;

const
  MY_SIZE = SizeOf(MyLong);
  BITS_IN_MYLONG = 8 * SizeOf(MyLong);

begin
  writeln('Size of MyLong: ', MY_SIZE);
  writeln('Bits in MyLong: ', BITS_IN_MYLONG);
end.
