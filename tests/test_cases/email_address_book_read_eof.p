program email_address_book_read_eof;

var
  datafile: text;
  line: string;
  linecounter: integer;

begin
  assign(datafile, 'tests/output/email_address_book.txt');
  reset(datafile);
  linecounter := 1;
  while eof(datafile) = false do
    begin
      readln(datafile, line);
      linecounter := linecounter + 1;
      if ((linecounter mod 2) = 0) then
        write('Name  ')
      else
        write('Email ');
      writeln((linecounter div 2), ': ', line);
      if ((linecounter mod 2) = 1) then
        writeln;
    end;
  close(datafile);
end.
