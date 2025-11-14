program DebugCase05ResetAndReopen;

uses SysUtils;

const
  FileName = 'dbg_case05.bin';

var
  F: file of Longint;
  PosValue: Longint;
begin
  Writeln('[CASE05] begin');
  Assign(F, FileName);
  {$I-}
  if FileExists(FileName) then
    DeleteFile(FileName);
  Reset(F);
  Writeln('[CASE05] reset_nonexistent IORes=', IOResult);
  Rewrite(F);
  Writeln('[CASE05] rewrite IORes=', IOResult);
  Close(F);
  Reset(F);
  Writeln('[CASE05] reset IORes=', IOResult);
  PosValue := FilePos(F);
  Writeln('[CASE05] filepos_reset=', PosValue);
  Close(F);
  Reset(F);
  Writeln('[CASE05] reset_again IORes=', IOResult, ' filepos=', FilePos(F));
  Close(F);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE05] done');
end.
