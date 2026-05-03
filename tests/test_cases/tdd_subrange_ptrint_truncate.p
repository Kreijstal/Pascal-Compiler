program repro_subrange;

{ Mimics oldheap.inc:151 — size : 0..high(ptrint);
  This is the actual declared type of the field that was being
  truncated to 32 bits in pp_bootstrap. }

type
  POsChunk = ^TOsChunk;
  TOsChunk = record
    size : 0..high(ptrint);     { subrange type — should be 8 bytes on x86-64 }
    next : pointer;
  end;

const
  ocrecycleflag = 1;
  HIGH_BIT_MARKER = $A400000000;

var
  hdr: TOsChunk;
  p: POsChunk;
begin
  hdr.size := HIGH_BIT_MARKER;
  hdr.next := nil;
  p := @hdr;

  p^.size := p^.size or ocrecycleflag;

  if hdr.size = (HIGH_BIT_MARKER or ocrecycleflag) then
    writeln('OK size=', hdr.size)
  else
    writeln('BUG size=', hdr.size, ' expected=', HIGH_BIT_MARKER or ocrecycleflag);
end.
