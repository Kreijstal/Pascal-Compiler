{ Test: SizeOf in const expressions }
{ KGPC does not support SizeOf in const expressions, but FPC does }
unit unit_sizeof_const;

{$mode objfpc}

interface

const
  IntSize = SizeOf(Integer);

implementation

end.
