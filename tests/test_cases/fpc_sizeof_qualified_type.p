{ FPC Bootstrap Gap: SizeOf() on qualified type names in const expressions
  FPC uses patterns like:
    { In baseunix.pp, the pattern is: }
    { type culong = UnixType.culong; }  (via macro expansion)
    { const BITSINWORD = 8*sizeof(culong); }
  
  This test checks if SizeOf works on type aliases that reference types
  from other units using the UnitName.TypeName syntax.
}
program fpc_sizeof_qualified_type;

type
  { Simple type alias - works }
  MyLong = LongInt;

const
  SIZE1 = SizeOf(MyLong);
  SIZE2 = SizeOf(LongInt);

begin
  writeln('SizeOf MyLong alias: ', SIZE1);
  writeln('SizeOf LongInt direct: ', SIZE2);
end.
