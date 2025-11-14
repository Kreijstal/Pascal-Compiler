program RtlDirOps;

uses SysUtils;

const
  BaseDir = 'rtl_dir_ops_tmp';
  FileA = BaseDir + '/file_a.txt';
  FileB = BaseDir + '/file_b.txt';

function BoolStr(Value: boolean): string;
begin
  if Value then
    BoolStr := 'TRUE'
  else
    BoolStr := 'FALSE';
end;

procedure Cleanup;
var
  Dummy: boolean;
begin
  if FileExists(FileB) then
    Dummy := DeleteFile(FileB);
  if FileExists(FileA) then
    Dummy := DeleteFile(FileA);
  if DirectoryExists(BaseDir) then
    RmDir(BaseDir);
end;

var
  F: text;
  DirCreated, Renamed, Deleted: boolean;
begin
  Cleanup;
  MkDir(BaseDir);
  DirCreated := DirectoryExists(BaseDir);

  assign(F, FileA);
  rewrite(F);
  writeln(F, 'hello');
  close(F);

  Renamed := RenameFile(FileA, FileB);
  Deleted := DeleteFile(FileB);
  Cleanup;

  WriteLn('DirCreated=', BoolStr(DirCreated));
  WriteLn('Renamed=', BoolStr(Renamed));
  WriteLn('Deleted=', BoolStr(Deleted));
  WriteLn('DirRemoved=', BoolStr(not DirectoryExists(BaseDir)));
end.
