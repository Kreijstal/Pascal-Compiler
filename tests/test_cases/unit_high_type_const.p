{ Test: High() in const expressions with type arguments }
{ KGPC does not support High(type) in const expressions, but FPC does }
{ This is needed for system.pp: MaxSIntValue = High(ValSInt) }
unit unit_high_type_const;

{$mode objfpc}

interface

const
  MaxInt64Value = High(Int64);
  MaxByteValue = High(Byte);
  MaxIntegerValue = High(Integer);

implementation

end.
