program tdd_sysutils_path_ops;

uses SysUtils;

{ Normalise the platform path separator so one .expected works on every
  target: IncludeTrailingPathDelimiter appends DirectorySeparator, which is
  '\' on Windows and '/' on POSIX.  This replace is a no-op on POSIX. }
procedure W(const S: AnsiString);
begin
  writeln(StringReplace(S, DirectorySeparator, '/', [rfReplaceAll]));
end;

begin
  { ExtractFileName }
  W(ExtractFileName('/home/user/test.pas'));
  W(ExtractFileName('test.pas'));
  W(ExtractFileName('/home/user/'));
  W(ExtractFileName(''));

  { ExtractFileExt }
  W(ExtractFileExt('/home/user/test.pas'));
  W(ExtractFileExt('noext'));
  W(ExtractFileExt(''));

  { ChangeFileExt }
  W(ChangeFileExt('test.pas', '.txt'));
  W(ChangeFileExt('test', '.txt'));
  W(ChangeFileExt('dir/test.pas', ''));

  { ExtractFilePath }
  W(ExtractFilePath('/home/user/test.pas'));
  W(ExtractFilePath('test.pas'));
  W(ExtractFilePath(''));

  { IncludeTrailingPathDelimiter }
  W(IncludeTrailingPathDelimiter('/home'));
  W(IncludeTrailingPathDelimiter('/home/'));

  { ExcludeTrailingPathDelimiter }
  W(ExcludeTrailingPathDelimiter('/home/'));
  W(ExcludeTrailingPathDelimiter('/'));
  W(ExcludeTrailingPathDelimiter(''));
end.
