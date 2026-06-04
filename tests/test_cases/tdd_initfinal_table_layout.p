{ Regression: KGPC must emit the program's INITFINAL symbol as a full
  TInitFinalTable header, not a bare 4-byte TableCount.

  FPC's system unit declares
      TInitFinalTable = record
        TableCount, InitCount : ALUUInt;   { each 8 bytes on x86_64 }
        Procs : array[...] of TInitFinalRec;
      end;
      InitFinalTable : TInitFinalTable; external name 'INITFINAL';
  and FPC_FINALIZEUNITS reads InitCount at offset 8 to drive the unit
  finalization loop (Procs[InitCount+1].FinalProc).

  KGPC inlines unit init/final into main, so it emits an empty table with
  TableCount = InitCount = 0.  It previously emitted only `.long 0` (4 bytes),
  leaving InitCount (offset 8) reading whatever data followed the symbol -- a
  garbage huge count that made FPC_FINALIZEUNITS dereference a wild FinalProc
  pointer and crash on exit (the FPC bootstrap "Unhandled exception raised
  with code <ptr>" after `system.pp` finished compiling).

  This test maps the same external record onto INITFINAL and verifies both
  leading native-word fields read back as 0.  Before the fix, the InitCount
  read picks up adjacent data and is nonzero. }
program tdd_initfinal_table_layout;

type
  TInitFinalHeader = record
    TableCount: QWord;
    InitCount: QWord;
  end;

var
  InitFinalTable: TInitFinalHeader; external name 'INITFINAL';

begin
  if InitFinalTable.TableCount <> 0 then
    WriteLn('BAD TableCount=', InitFinalTable.TableCount)
  else if InitFinalTable.InitCount <> 0 then
    WriteLn('BAD InitCount=', InitFinalTable.InitCount)
  else
    WriteLn('INITFINAL header zeroed');
end.
