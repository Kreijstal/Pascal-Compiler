program email_address_book;

uses crt;

var
  datafile: text;
  counter: integer;
  name: string;
  email: string;

begin
  clrscr;
  assign(datafile, 'tests/output/email_address_book.txt');
  rewrite(datafile);
  for counter := 1 to 3 do
    begin
      writeln('Enter name ', counter, ' out of 3');
      readln(name);
      writeln('Enter that person''s email.');
      readln(email);
      writeln(datafile, name);
      writeln(datafile, email);
    end;
  close(datafile);
end.
