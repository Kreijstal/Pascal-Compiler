program pshortint_deref_test;
{ Regression: dereferencing a pointer to a 1-byte signed type (^ShortInt)
  must sign-extend the loaded byte.  The pointer-deref leaf hardcoded a
  zero-extending 1-byte load, so p^ yielded 224 instead of -32 for negative
  values, while record/array accesses of the same type sign-extended
  correctly. }
type
  pshort = ^shortint;
var
  s: shortint;
  p: pshort;
begin
  s := -32;
  p := @s;
  writeln(p^);
  if p^ < 0 then
    writeln('negative')
  else
    writeln('non-negative');
end.
