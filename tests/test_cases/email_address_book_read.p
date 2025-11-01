program email_address_book_read;

var
  datafile: text;
  counter: integer;
  line: string;

begin
  assign(datafile, 'tests/output/email_address_book.txt');
  reset(datafile);
  for counter := 1 to 3 do
    begin
      readln(datafile, line);
      writeln('Name  ', counter, ': ', line);
      readln(datafile, line);
      writeln('Email ', counter, ': ', line);
      writeln;
    end;
  close(datafile);
end.
