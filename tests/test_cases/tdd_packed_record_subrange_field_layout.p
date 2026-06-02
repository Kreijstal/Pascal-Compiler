program tdd_packed_record_subrange_field_layout;
{ Regression: a packed-record field whose type is a user *subrange* alias
  (e.g. `-32768..32767`, the way FPC's system.pp declares SmallInt) must
  occupy its true width.  Such an alias has no explicit storage_size at
  creation -- it is populated lazily from the range bounds during semcheck.
  If a field of this type was sized before that lazy pass ran, the field-size
  computation fell back to the 4-byte INT_TYPE tag and cached it permanently,
  so every later layout consumer (sizeof, field-offset resolution) read the
  poisoned 4.  That over-sized FPC's ogcoff.pas coffsymbol from 18 to 20 bytes
  and corrupted the COFF symbol-table stride in the KGPC-built FPC linker.
  narrowing_alias_storage_size now derives the width from the known range
  bounds when storage_size is still 0, so the very first sizing is correct
  regardless of evaluation order. }
type
  tsec = -32768..32767;   { signed 16-bit subrange -> 2 bytes }
  tsho = -128..127;       { signed 8-bit subrange  -> 1 byte }
  TCoffSym = packed record
    name : array[0..3] of char;
    sp   : longword;
    v    : longword;
    sec  : tsec;
    e    : word;
    t    : byte;
    aux  : byte;
  end;
  TSec = packed record b: tsec end;
  TSho = packed record b: tsho end;
var
  s: TCoffSym;
begin
  writeln(sizeof(TSec));     { 2 }
  writeln(sizeof(TSho));     { 1 }
  writeln(sizeof(TCoffSym)); { 18 }
  { Writing each field must land at its own offset and not clobber neighbours. }
  s.sec := -2;
  s.e := 4660;
  s.t := 86;
  s.aux := 120;
  writeln(s.sec, ' ', s.e, ' ', s.t, ' ', s.aux); { -2 4660 86 120 }
end.
