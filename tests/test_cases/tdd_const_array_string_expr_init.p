program tdd_const_array_string_expr_init;

const
  ArMagic: array[1..8] of char = '!<arch>'#10;

begin
  WriteLn(ArMagic[1], ArMagic[7], Ord(ArMagic[8]));
end.
