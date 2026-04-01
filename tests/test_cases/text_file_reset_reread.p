{$H+}
program text_file_reset_reread;
{
  Regression test: reset(f) on an already-open text file must rewind
  to the beginning so subsequent reads produce the same data again.
  This is the pattern used by FPC's msg2inc utility (two-pass parsing).
}
var
  f : text;
  s : string;
  count : longint;
  tmpname : string;
begin
  tmpname := 'test_reset_reread_tmp.txt';

  { Create test file }
  assign(f, tmpname);
  rewrite(f);
  writeln(f, 'alpha');
  writeln(f, 'beta');
  writeln(f, 'gamma');
  close(f);

  { Pass 1: read all lines }
  assign(f, tmpname);
  reset(f);
  count := 0;
  while not eof(f) do
  begin
    readln(f, s);
    inc(count);
  end;
  writeln('Pass 1: ', count, ' lines');

  { Pass 2: reset (without close) and re-read }
  reset(f);
  count := 0;
  while not eof(f) do
  begin
    readln(f, s);
    inc(count);
    writeln('  ', s);
  end;
  writeln('Pass 2: ', count, ' lines');

  close(f);
end.
