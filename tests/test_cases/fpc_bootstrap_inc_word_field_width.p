program fpc_bootstrap_inc_word_field_width;

{$mode objfpc}
{$R-}

type
  TState = record
    maxreginfo: word;
    maxreginfoinc: word;
    maxreg: word;
    sentinel: word;
  end;

var
  s: TState;

begin
  s.maxreginfo := 65530;
  s.maxreginfoinc := 16;
  s.maxreg := 65535;
  s.sentinel := $55aa;

  inc(s.maxreginfo, s.maxreginfoinc);
  inc(s.maxreg);

  if (s.maxreginfo = 10) and
     (s.maxreginfoinc = 16) and
     (s.maxreg = 0) and
     (s.sentinel = $55aa) then
    writeln('ok')
  else
    writeln('bad ', s.maxreginfo, ' ', s.maxreginfoinc, ' ',
            s.maxreg, ' ', s.sentinel);
end.
