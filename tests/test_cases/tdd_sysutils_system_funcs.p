program tdd_sysutils_system_funcs;

uses SysUtils;

var
  tick1, tick2: QWord;
  curdir: AnsiString;
  envval: AnsiString;
  pid: LongInt;
begin
  { GetTickCount64 basic check }
  tick1 := GetTickCount64;
  tick2 := GetTickCount64;
  if tick2 >= tick1 then
    writeln('GetTickCount64: OK')
  else
    writeln('GetTickCount64: FAIL');

  { GetCurrentDir }
  curdir := GetCurrentDir;
  if Length(curdir) > 0 then
    writeln('GetCurrentDir: OK')
  else
    writeln('GetCurrentDir: FAIL');

  { FileExists }
  writeln(FileExists('/dev/null'));
  writeln(FileExists('/nonexistent_file_xyz'));

  { DirectoryExists }
  writeln(DirectoryExists('/tmp'));
  writeln(DirectoryExists('/nonexistent_dir_xyz'));

  { GetEnvironmentVariable }
  envval := GetEnvironmentVariable('PATH');
  if Length(envval) > 0 then
    writeln('GetEnvVar: OK')
  else
    writeln('GetEnvVar: FAIL');

  { GetProcessID }
  pid := GetProcessID;
  if pid > 0 then
    writeln('GetProcessID: OK')
  else
    writeln('GetProcessID: FAIL');
end.
