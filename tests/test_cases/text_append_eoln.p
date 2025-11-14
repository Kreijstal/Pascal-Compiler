program TextAppendEoln;

var
  F: text;
  Line: string;

function BoolStr(Value: boolean): string;
begin
  if Value then
    BoolStr := 'TRUE'
  else
    BoolStr := 'FALSE';
end;

begin
  assign(F, 'tests/output/text_append_eoln.txt');
  rewrite(F);
  writeln(F, 'Hello');
  close(F);

  append(F);
  writeln(F, 'World');
  close(F);

  reset(F);
  writeln('EOLN_INITIAL=', BoolStr(eoln(F)));
  while not eof(F) do
  begin
    readln(F, Line);
    writeln('LINE=', Line);
  end;
  close(F);
end.
