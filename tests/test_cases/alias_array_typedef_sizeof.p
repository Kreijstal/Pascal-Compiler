program alias_array_typedef_sizeof;

type
  culong = 0..18446744073709551615;
  sigset_t = array[0..15] of culong;
  tsigset = sigset_t;

begin
  if SizeOf(sigset_t) <> 128 then
    writeln('bad sigset');
  if SizeOf(tsigset) <> 128 then
    writeln('bad tsigset');
  writeln('ok');
end.
