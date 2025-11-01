program text_file_roundtrip;

var
  data: text;
  firstLine: string;
  secondLine: string;
  line: string;
  index: integer;

begin
  readln(firstLine);
  readln(secondLine);

  assign(data, 'tests/output/text_roundtrip.txt');
  rewrite(data);
  writeln(data, firstLine);
  writeln(data, secondLine);
  close(data);

  reset(data);
  index := 0;
  while not eof(data) do
    begin
      readln(data, line);
      index := index + 1;
      writeln('FILE', index, ':', line);
    end;
  close(data);

  readln;
end.
