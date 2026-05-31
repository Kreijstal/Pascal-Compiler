{ Regression: High(QWord) must yield the full unsigned bit pattern
  $FFFFFFFFFFFFFFFF, not INT64_MAX.  fpc_val_int64_shortstr uses
  `lim := High(lim)` (lim: QWord) as an unsigned overflow limit; a truncated
  High cleared the high bit and made every $8.. hex literal fail to parse
  ("Error converting hexadecimal"), which broke the FPC RTL self-host. }
var
  lim, prev: qword;
begin
  lim := high(qword);
  prev := $0FFFFFFFFFFFFFFF;
  writeln(lim);
  writeln(low(qword));
  if prev > lim div 16 then
    writeln('overflow')
  else
    writeln('ok');
end.
