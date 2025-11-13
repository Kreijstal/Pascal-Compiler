program sysutils_extras;

uses SysUtils;

var
  val: Real;
  intv: Longint;
begin
  writeln(FloatToStr(123.456));
  writeln(StrToFloat('3.14'));
  if TryStrToFloat('bad', val) then
    writeln('ok')
  else
    writeln('bad');
  if TryStrToInt('42', intv) then
    writeln('ok', ' ', intv)
  else
    writeln('fail');
  writeln(AnsiCompareStr('abc', 'abc'));
  writeln(AnsiCompareStr('abc', 'abd'));
  writeln(AnsiPos('bc', 'abcdef'));
  writeln(ExtractFilePath('dir/sub/test.p'));
  writeln(IncludeTrailingPathDelimiter('dir'));
  writeln(FileExists('tests/test_cases/format_function.p'));
  writeln(DirectoryExists('tests'));
end.
