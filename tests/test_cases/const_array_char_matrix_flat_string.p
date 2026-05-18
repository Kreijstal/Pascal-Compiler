{ Regression test for the FPC msgtxt-style flat-string typed-const for a 2-D
  char array.  KGPC must split the concatenated initializer into the outer
  dimension's rows (length = inner-dim-size each) and emit per-(outer,inner)
  byte assigns; not reject the input outright, and not write all bytes into
  the linear m[i] slot (which would corrupt later rows' storage).  See
  msgtxt.inc: `const msgtxt : array[0..000398, 1..240] of char =
  ('01000_T_...'#0+...);`. }
program const_array_char_matrix_flat_string;

const
  m : array[0..2, 1..4] of char =
    ('abcd'+'EFGH'+'wxyz');

begin
  writeln(m[0,1], m[0,2], m[0,3], m[0,4]);
  writeln(m[1,1], m[1,2], m[1,3], m[1,4]);
  writeln(m[2,1], m[2,2], m[2,3], m[2,4]);
end.
