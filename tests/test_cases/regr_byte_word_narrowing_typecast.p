program regr_byte_word_narrowing_typecast;

var
  source: LongWord;
  wide: LongInt;
begin
  source := $1234;
  wide := byte(source) xor byte($56);
  writeln(wide);

  source := $12345678;
  wide := word(source) xor word($00FF);
  writeln(wide);
end.
