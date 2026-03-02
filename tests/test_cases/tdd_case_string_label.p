{$mode objfpc}
program tdd_case_string_label;

var
  s: string;

begin
  s := 'TRUE';
  case s of
    'NIL': writeln('nil');
    'FALSE': writeln('false');
    'TRUE': writeln('true');
  else
    writeln('other');
  end;
end.
