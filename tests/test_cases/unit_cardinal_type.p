{ Test: Cardinal type alias }
{ KGPC does not define Cardinal as a builtin type alias, but FPC does }
{ Cardinal is an unsigned 32-bit integer (alias for LongWord) }
unit unit_cardinal_type;

{$mode objfpc}

interface

var
  x: Cardinal;

implementation

end.
