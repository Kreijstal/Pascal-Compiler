{ Regression: a record's alignment is the maximum alignment of its members,
  not a value derived from its total size.  FILETIME = record lo, hi: DWORD end
  has size 8 but alignment 4 (its widest member is a 4-byte DWORD).  KGPC used
  to derive a cached record's alignment from its size (8 -> align 8), which
  over-aligned such records: under {$PACKRECORDS C}, the three FILETIME fields
  in WIN32_FIND_DATAW pushed cFileName from offset 44 to 48, so FindFirstFileW
  filenames were read 2 widechars short and the Win64 compiler could not locate
  any include directory.  Mirrors that layout. }
program tdd_nested_record_alignment;
{$PACKRECORDS C}
type
  TFileTime = record
    lo: longword;
    hi: longword;
  end;
  TFindData = record
    attrs    : longword;                  { offset 0 }
    t1       : TFileTime;                 { offset 4  (align 4, not 8) }
    t2       : TFileTime;                 { offset 12 }
    t3       : TFileTime;                 { offset 20 }
    sizeHigh : longword;                  { offset 28 }
    sizeLow  : longword;                  { offset 32 }
    r0       : longword;                  { offset 36 }
    r1       : longword;                  { offset 40 }
    cFileName: array[0..259] of widechar; { offset 44 }
  end;
var
  r: TFindData;
  ok: boolean;
begin
  ok := true;
  if (PtrUInt(@r.t1) - PtrUInt(@r)) <> 4 then ok := false;
  if (PtrUInt(@r.cFileName) - PtrUInt(@r)) <> 44 then ok := false;
  if SizeOf(TFileTime) <> 8 then ok := false;
  if ok then WriteLn('OK')
  else WriteLn('FAIL t1=', PtrUInt(@r.t1) - PtrUInt(@r),
               ' cFileName=', PtrUInt(@r.cFileName) - PtrUInt(@r));
end.
