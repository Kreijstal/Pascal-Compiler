{ Regression test: SetString with shortstring target
  Verifies that SetString accepts a shortstring variable as
  its first argument, not just AnsiString.

  This pattern is used in heaptrc.pp.
}
{$mode objfpc}
program regression_setstring_shortstring;

var
  s: String[80];
  src: String;
begin
  src := 'Hello World';
  SetString(s, @src[1], 5);
  WriteLn(s);
end.
