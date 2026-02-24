program fpc_bootstrap_heapinc_commonheader_sizeof_repro;
{$mode objfpc}

type
  THeapInc = object
    type
      CommonHeader = record
        h: PtrUInt;
      end;
      pCommonHeader = ^CommonHeader;
      FixedArena = record
        hdr: pCommonHeader;
      end;
    const
      CommonHeaderSize = sizeof(CommonHeader);
      FixedArenaDataOffset = sizeof(FixedArena) + CommonHeaderSize;
  end;

begin
end.
