program builtins_64bit;
uses ctypes;
var
  q: QWord;
  i64: Int64;
  n: NativeUInt;
begin
  q := 10;
  i64 := -20;
  n := 30;

  writeln('q_sqr=', sqr(q));
  writeln('i64_sqr=', sqr(i64));
  writeln('n_sqr=', sqr(n));

  writeln('q_odd=', odd(q));
  writeln('i64_odd=', odd(i64));
  writeln('n_odd=', odd(n));

  writeln('q_chr=', ord(chr(q)));
  writeln('i64_chr=', ord(chr(65)));

  writeln('q_ord=', ord(q));
  writeln('i64_ord=', ord(i64));
  writeln('n_ord=', ord(n));

  writeln('q_abs=', abs(q));
  writeln('i64_abs=', abs(i64));
  writeln('n_abs=', abs(n));

  writeln('trunc=', trunc(1.9));
  writeln('round=', round(1.9));
end.
