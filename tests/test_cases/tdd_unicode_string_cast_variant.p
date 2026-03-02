program tdd_unicode_string_cast_variant;

{$mode objfpc}

var
  v: Variant;
  s: UnicodeString;

begin
  v := 'hi';
  s := UnicodeString(v);
  writeln(s);
end.
