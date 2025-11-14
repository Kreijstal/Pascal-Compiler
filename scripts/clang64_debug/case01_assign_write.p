program DebugCase01FilePosReset;

uses SysUtils;

const
  FileName = 'dbg_case01.bin';

var
  F: file of Longint;
  PosValue: Longint;
begin
  Writeln('[CASE01] begin');
  Assign(F, FileName);
  {$I-}
  Rewrite(F);
  Writeln('[CASE01] rewrite IORes=', IOResult);
  Close(F);
  Writeln('[CASE01] close_after_rewrite IORes=', IOResult);
  Reset(F);
  Writeln('[CASE01] reset IORes=', IOResult);
  PosValue := FilePos(F);
  Writeln('[CASE01] filepos_after_reset=', PosValue, ' IORes=', IOResult);
  Seek(F, 0);
  Writeln('[CASE01] seek0 IORes=', IOResult, ' filepos=', FilePos(F));
  Close(F);
  Writeln('[CASE01] final_close IORes=', IOResult);
  DeleteFile(FileName);
  {$I+}
  Writeln('[CASE01] done');
end.
