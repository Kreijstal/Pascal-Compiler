{ Regression: FPC's rtl/win/wininc/func.inc uses the OFS() intrinsic, e.g.
    IMAGE_FIRST_SECTION :=
      PIMAGE_SECTION_HEADER(OFS(ntheader^.OptionalHeader)
                            + ntheader^.FileHeader.SizeOfOptionalHeader);

  On flat 32/64-bit memory models (the only ones KGPC targets) OFS(x) is
  defined to be the linear address of x — i.e. PtrUInt(@x).  This test
  verifies that OFS yields the same value as the equivalent explicit
  QWord(@field) and PtrUInt(@field) expressions for a record field. }
{$mode objfpc}
program OfsBuiltin;

type
  TInner = record
    a: LongInt;
    b: LongInt;
  end;
  TOuter = record
    header: LongInt;
    inner: TInner;
  end;

var
  outer: TOuter;
  via_ofs, via_addr, via_ptruint: QWord;
begin
  via_ofs     := OFS(outer.inner);
  via_addr    := QWord(@outer.inner);
  via_ptruint := PtrUInt(@outer.inner);
  if (via_ofs = via_addr) and (via_ofs = via_ptruint) then
    Writeln('OK')
  else
    Writeln('MISMATCH');
end.
