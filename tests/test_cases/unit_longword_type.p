{ Test: LongWord type alias }
{ KGPC does not define LongWord as a builtin type alias, but FPC does }
{ LongWord is an unsigned 32-bit integer, equivalent to Cardinal/DWord }
unit unit_longword_type;

{$mode objfpc}

interface

var
  x: LongWord;
  y: Cardinal;

implementation

end.
