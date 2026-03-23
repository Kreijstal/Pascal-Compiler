{$mode objfpc}
program test_pword_punicodechar;
type
  TMyWord = Word;
  TMyWordArray = array of TMyWord;
  PUC = ^UnicodeChar;
var
  data: TMyWordArray;
  p: PUC;
begin
  SetLength(data, 2);
  data[0] := 65;
  data[1] := 0;
  p := @data[0];
  WriteLn(p^);
end.
