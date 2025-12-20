{ Test: Low() in const expressions with type arguments }
{ KGPC does not support Low(type) in const expressions, but FPC does }
{ This is needed for system.pp }
unit unit_low_type_const;

{$mode objfpc}

interface

const
  MinInt64Value = Low(Int64);
  MinIntegerValue = Low(Integer);

implementation

end.
