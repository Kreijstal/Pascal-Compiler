program tdd_sysutils_path_ops;

uses SysUtils;

begin
  { ExtractFileName }
  writeln(ExtractFileName('/home/user/test.pas'));
  writeln(ExtractFileName('test.pas'));
  writeln(ExtractFileName('/home/user/'));
  writeln(ExtractFileName(''));

  { ExtractFileExt }
  writeln(ExtractFileExt('/home/user/test.pas'));
  writeln(ExtractFileExt('noext'));
  writeln(ExtractFileExt(''));

  { ChangeFileExt }
  writeln(ChangeFileExt('test.pas', '.txt'));
  writeln(ChangeFileExt('test', '.txt'));
  writeln(ChangeFileExt('dir/test.pas', ''));

  { ExtractFilePath }
  writeln(ExtractFilePath('/home/user/test.pas'));
  writeln(ExtractFilePath('test.pas'));
  writeln(ExtractFilePath(''));

  { IncludeTrailingPathDelimiter }
  writeln(IncludeTrailingPathDelimiter('/home'));
  writeln(IncludeTrailingPathDelimiter('/home/'));

  { ExcludeTrailingPathDelimiter }
  writeln(ExcludeTrailingPathDelimiter('/home/'));
  writeln(ExcludeTrailingPathDelimiter('/'));
  writeln(ExcludeTrailingPathDelimiter(''));
end.
