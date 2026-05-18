program TypedConstRecordProcFieldStaticInit;

{ Regression test: a typed-const record whose field values are address-of
  procedure expressions must be emitted as a static .data block.  Before the
  fix the parser lowered the record initialiser to a runtime field-by-field
  assignment compound, which overrode any prior contents — in particular it
  clobbered C-side constructor initialisations of fields like MemoryManager.
  After the fix the storage is filled at link time and no runtime stores are
  emitted from the program body for the typed-const record itself. }

type
  TMyHandler = record
    NeedLock: Boolean;
    Tag: LongInt;
    GetMem: function(Size: NativeUInt): Pointer;
    FreeMem: function(P: Pointer): NativeUInt;
  end;

function MyGetMem(Size: NativeUInt): Pointer;
begin
  MyGetMem := nil;
end;

function MyFreeMem(P: Pointer): NativeUInt;
begin
  MyFreeMem := 0;
end;

const
  Handler: TMyHandler = (
    NeedLock: false;
    Tag: 4242;
    GetMem: @MyGetMem;
    FreeMem: @MyFreeMem;
  );

begin
  if Handler.NeedLock then
    WriteLn('locked')
  else
    WriteLn('unlocked');
  WriteLn('tag=', Handler.Tag);
  if Handler.GetMem = @MyGetMem then
    WriteLn('getmem-ok')
  else
    WriteLn('getmem-bad');
  if Handler.FreeMem = @MyFreeMem then
    WriteLn('freemem-ok')
  else
    WriteLn('freemem-bad');
end.
