{$mode objfpc}
program tdd_widechar_func_result;

{ Test that a function returning WideChar can assign its result
  from a WideChar cast expression. Validates WideChar is treated
  as a char type, not as Word. }

function CharToUChar(const c: AnsiChar): WideChar;
begin
  Result := WideChar(Ord(c));
end;

var
  wc: WideChar;
begin
  wc := CharToUChar('A');
  if wc = 'A' then
    writeln('PASS')
  else
    writeln('FAIL');
end.
