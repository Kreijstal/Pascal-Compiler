{ Test: SizeOf in array bounds }
{ KGPC does not support SizeOf in array bounds, but FPC does }
unit unit_sizeof_array_bounds;

{$mode objfpc}

interface

type
  TMyRecord = record
    Data: array[1..3 * SizeOf(Integer)] of Byte;
  end;

implementation

end.
