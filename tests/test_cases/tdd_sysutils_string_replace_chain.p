program tdd_sysutils_string_replace_chain;

uses SysUtils;

var
  s, t, u: AnsiString;
  p: Integer;
  flags: TReplaceFlags;
begin
  { Test 1: Basic StringReplace - single occurrence (first match only) }
  flags := [];
  s := StringReplace('Hello World', 'World', 'Pascal', flags);
  writeln(s);

  { Test 2: StringReplace with rfReplaceAll }
  flags := [rfReplaceAll];
  s := StringReplace('aaa bbb aaa', 'aaa', 'ccc', flags);
  writeln(s);

  { Test 3: StringReplace with rfIgnoreCase }
  flags := [rfIgnoreCase];
  s := StringReplace('Hello HELLO hello', 'hello', 'HI', flags);
  writeln(s);

  { Test 4: StringReplace with both flags }
  flags := [rfReplaceAll, rfIgnoreCase];
  s := StringReplace('Hello HELLO hello', 'hello', 'HI', flags);
  writeln(s);

  { Test 5: Chained string operations }
  s := '  Hello World  ';
  flags := [];
  t := Trim(StringReplace(UpperCase(s), 'WORLD', 'PASCAL', flags));
  writeln(t);

  { Test 6: Pos() used to extract substrings }
  s := 'key=value';
  p := Pos('=', s);
  if p > 0 then
  begin
    t := Copy(s, 1, p - 1);
    u := Copy(s, p + 1, Length(s) - p);
    writeln('Key: ', t);
    writeln('Value: ', u);
  end;

  { Test 7: CompareText chains }
  s := 'Alpha';
  if CompareText(s, 'ALPHA') = 0 then
    writeln('CompareText eq: OK')
  else
    writeln('CompareText eq: FAIL');
  
  if CompareText('aaa', 'bbb') < 0 then
    writeln('CompareText lt: OK')
  else
    writeln('CompareText lt: FAIL');

  { Test 8: SameText }
  if SameText('Pascal', 'PASCAL') then
    writeln('SameText: OK')
  else
    writeln('SameText: FAIL');

  { Test 9: Format with multiple types }
  s := Format('Name: %s, Age: %d, Score: %.1f', ['Pascal', 42, 98.5]);
  writeln(s);

  { Test 10: Multiple chained replaces }
  s := 'The quick brown fox jumps over the lazy dog';
  flags := [];
  s := StringReplace(s, 'quick', 'slow', flags);
  s := StringReplace(s, 'brown', 'white', flags);
  s := StringReplace(s, 'lazy', 'active', flags);
  writeln(s);

  { Test 11: Replace with empty string (deletion) }
  flags := [];
  s := StringReplace('Hello World', 'World', '', flags);
  writeln('[', Trim(s), ']');

  { Test 12: Replace nonexistent pattern }
  flags := [];
  s := StringReplace('Hello', 'XYZ', 'ABC', flags);
  writeln(s);

  { Test 13: IntToStr/StrToInt combined }
  p := StrToInt(IntToStr(42)) + StrToInt(IntToStr(58));
  writeln('Sum: ', p);

  { Test 14: Pos with no match }
  p := Pos('xyz', 'Hello World');
  writeln('NoMatch: ', p);

  { Test 15: IsDelimiter }
  s := 'Hello, World!';
  if IsDelimiter(',!', s, 6) then
    writeln('IsDelimiter comma: OK')
  else
    writeln('IsDelimiter comma: FAIL');
  if not IsDelimiter(',!', s, 1) then
    writeln('IsDelimiter H: OK')
  else
    writeln('IsDelimiter H: FAIL');

  { Test 16: Complex expression with multiple string ops }
  s := '  VALUE=42  ';
  t := Trim(s);
  p := Pos('=', t);
  u := LowerCase(Copy(t, 1, p - 1));
  writeln('LowerKey: ', u);
  writeln('NumVal: ', StrToInt(Copy(t, p + 1, Length(t) - p)));
end.
