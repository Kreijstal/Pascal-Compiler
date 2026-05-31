{ Regression: an enumeration's default storage size is 4 bytes (FPC's default
  {$PACKENUM 4}/{$Z4}), not 1.  KGPC used to size every enum with <=256 literals
  as a single byte, which desynchronised the byte offset of any record/typed-
  const field declared after an enum field versus FPC.  This is exactly what
  broke FPC's global `target_info : tsysteminfo` record: with enums sized at 1
  byte, the `first_parm_offset` field landed 32 bytes too early, so the
  syscall-wrapper stack-parameter offset (target_info.first_parm_offset = 16)
  read back as 0, mmap got a garbage offset argument, the heap manager's
  SysOSAlloc returned nil, and every pp_bootstrap-compiled program died with
  RTE 203 on the first heap allocation.

  Mirrors the tricky prefix of tsysteminfo: a set field, several shortstring
  fields, a char, several enum fields, a packed sub-record, then a longint.
  Both the field offsets and the typed-const values must match FPC. }
program tdd_enum_field_size;
type
  TFlag = (f00, f01, f02, f03, f04);
  TFlagSet = set of TFlag;
  TSmallEnum = (en0, en1, en2);
  TAlign = packed record
    a, b, c: longint;
  end;
  TInfo = record
    flags     : TFlagSet;      { set of small enum: 4 bytes }
    name      : string[14];
    ext       : string[5];
    dirsep    : char;
    asmk      : TSmallEnum;     { enum: 4 bytes, must align to 4 }
    link      : TSmallEnum;
    endian    : TSmallEnum;
    alignment : TAlign;
    first_parm_offset : longint;
    stacksize : longint;
  end;
const
  k: TInfo = (
    flags: [];
    name: 'x86_64-linux';
    ext: '.s';
    dirsep: '/';
    asmk: en1;
    link: en2;
    endian: en0;
    alignment: (a: 0; b: 0; c: 0);
    first_parm_offset: 16;
    stacksize: 8192;
  );
var
  r: TInfo;
  ok: boolean;
begin
  ok := true;
  { A plain enum is 4 bytes by default. }
  if SizeOf(TSmallEnum) <> 4 then ok := false;
  { An enum field aligns to its 4-byte natural alignment. }
  if ((PtrUInt(@r.asmk) - PtrUInt(@r)) mod 4) <> 0 then ok := false;
  { The longint after the enum/packed-record prefix must read back its
    typed-const value, not 0. }
  if k.first_parm_offset <> 16 then ok := false;
  if k.stacksize <> 8192 then ok := false;
  if ord(k.asmk) <> 1 then ok := false;
  if ord(k.link) <> 2 then ok := false;
  if ord(k.endian) <> 0 then ok := false;
  if ok then WriteLn('OK')
  else WriteLn('FAIL fpo=', k.first_parm_offset,
               ' ss=', k.stacksize,
               ' asmk_off=', PtrUInt(@r.asmk) - PtrUInt(@r),
               ' dirsep_off=', PtrUInt(@r.dirsep) - PtrUInt(@r));
end.
