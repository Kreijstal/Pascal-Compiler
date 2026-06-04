program packed_record_smallint_field_size;
{ Regression: a SmallInt/ShortInt field in a packed record must occupy its
  true width (2 / 1 bytes), not the 4-byte width of the generic INT_TYPE tag
  these types are recorded with (they have no dedicated narrow signed tag, so
  the authoritative width lives in the alias storage_size).  Mis-sizing the
  field to 4 bytes is what bloated FPC's ogcoff.pas coffsymbol record from 18
  to 20 bytes, so every COFF object the KGPC-built FPC compiler emitted had a
  wrong NumberOfSymbols/string-table offset and could not be linked. }
type
  TCoffSym = packed record
    name : array[0..3] of char;
    sp   : longword;
    v    : longword;
    sec  : smallint;
    e    : word;
    t    : byte;
    aux  : byte;
  end;
  TSmall = packed record b: smallint end;
  TShort = packed record b: shortint end;
var
  s: TCoffSym;
begin
  writeln(sizeof(TSmall));   { 2 }
  writeln(sizeof(TShort));   { 1 }
  writeln(sizeof(TCoffSym)); { 18 }
  { Field access must use the field's true width and not clobber neighbours. }
  s.sec := -2;
  s.e := 4660;
  s.t := 86;
  s.aux := 120;
  writeln(s.sec, ' ', s.e, ' ', s.t, ' ', s.aux); { -2 4660 86 120 }
end.
