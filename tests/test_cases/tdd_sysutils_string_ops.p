program tdd_sysutils_string_ops;

uses SysUtils;

begin
  { IntToStr / StrToInt roundtrip }
  writeln(IntToStr(42));
  writeln(IntToStr(-7));
  writeln(IntToStr(0));
  writeln(StrToInt('123'));
  writeln(StrToInt('-456'));

  { UpperCase / LowerCase }
  writeln(UpperCase('hello'));
  writeln(LowerCase('WORLD'));
  writeln(UpperCase(''));
  writeln(LowerCase(''));

  { Trim / TrimLeft / TrimRight }
  writeln('[', Trim('  hi  '), ']');
  writeln('[', TrimLeft('  hi  '), ']');
  writeln('[', TrimRight('  hi  '), ']');
  writeln('[', Trim(''), ']');

  { CompareText (case-insensitive) }
  writeln(CompareText('abc', 'ABC'));
  writeln(CompareText('a', 'b') < 0);
  writeln(CompareText('b', 'a') > 0);

  { SameText (case-insensitive equality) }
  writeln(SameText('Hello', 'hELLO'));
  writeln(SameText('abc', 'def'));

  { AnsiUpperCase / AnsiLowerCase }
  writeln(AnsiUpperCase('test'));
  writeln(AnsiLowerCase('TEST'));
end.
