program fpc_bootstrap_inc_qword_longword_delta;

var
  q: QWord;
  delta: LongWord;

begin
  q := 1;
  delta := LongWord($FFFFFFFF);
  Inc(q, delta);
  if q = QWord(4294967296) then
    WriteLn('ok')
  else
    WriteLn('bad');
end.
