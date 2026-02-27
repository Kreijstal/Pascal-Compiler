program pstring_upcase_index;

type
  PString = ^ShortString;
  TRec = record
    s: PString;
  end;

var
  str: ShortString;
  rec: TRec;
  i: Integer;

begin
  str := 'Abc';
  rec.s := @str;
  i := 2;
  if UpCase(rec.s^[i]) = 'B' then
    writeln('ok')
  else
    writeln('bad');
end.
