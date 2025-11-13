program sysutils_paths;

uses SysUtils;

procedure Print(const S: AnsiString);
begin
    Writeln(S);
end;

begin
    Print('Name1=' + ExtractFileName('dir/sub/test.p'));
    Print('Name2=' + ExtractFileName('test'));
    Print('Ext1=' + ExtractFileExt('dir/sub/test.p'));
    Print('Ext2=' + ExtractFileExt('dir/sub/test'));
    Print('Change1=' + ChangeFileExt('dir/sub/test.p', '.txt'));
    Print('Change2=' + ChangeFileExt('dir/sub/test', '.txt'));
    Print('Change3=' + ChangeFileExt('dir/sub/test.p', ''));
    Print('Exclude1=' + ExcludeTrailingPathDelimiter('dir/sub/'));
    Print('Exclude2=' + ExcludeTrailingPathDelimiter('/'));
    Print('Exclude3=' + ExcludeTrailingPathDelimiter('C:\'));
    Print('Include=' + IncludeTrailingPathDelimiter(ExcludeTrailingPathDelimiter('dir/sub/')));
    Print('Path=' + ExtractFilePath('dir/sub/test.p'));
end.
