program const_array_string_init;

{$mode objfpc}

const
  HexDigits: array[0..15] of AnsiChar = '0123456789ABCDEF';
  HexDigitsW: array[0..15] of WideChar = '0123456789ABCDEF';

begin
  writeln(HexDigits[0], HexDigits[15]);
  writeln(Ord(HexDigitsW[0]), ' ', Ord(HexDigitsW[15]));
end.
