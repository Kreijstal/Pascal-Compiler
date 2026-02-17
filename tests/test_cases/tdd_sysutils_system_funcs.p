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

  { FileExists — use the meson build file which must exist in curdir }
  writeln(FileExists('meson.build'));
  writeln(FileExists('nonexistent_file_xyz_9999'));

  { DirectoryExists — use 'tests' subdir which must exist }
  writeln(DirectoryExists('tests'));
  writeln(DirectoryExists('nonexistent_dir_xyz_9999'));

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
