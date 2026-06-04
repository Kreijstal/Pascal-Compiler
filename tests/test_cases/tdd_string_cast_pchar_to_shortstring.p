program tdd_string_cast_pchar_to_shortstring;
{$mode objfpc}{$H+}

{ Regression: `ShortString := string(PChar)` must materialise the PChar as a
  real managed AnsiString (StrPas) instead of passing the raw C-string
  pointer through.  The old lowering left a bare C-string that downstream
  reached kgpc_string_to_shortstring, whose C-string heuristic guesses the
  ShortString length from src[0] and silently drops the first character
  whenever (unsigned char)src[0] == strlen-1.

  A name 47 bytes long starting with '.' (0x2E = 46) hits that coincidence
  exactly, so the leading '.' is dropped.  This is FPC's ogcoff.pas Read_str
  (`secname := string(PChar(@FCoffStrs[..]))`): the lost '.' made a COFF
  section name miss the Win64 link-script glob -> Internal error 202102001
  in the KGPC->FPC Windows self-host.

  The string below is exactly 47 chars and begins with '.', reproducing the
  coincidence; it must round-trip with its leading '.' intact. }

type
  TSymStr = ShortString;
var
  buf: array[0..63] of char;
  p: pchar;
  s: TSymStr;
  src: ansistring;
  i: integer;
begin
  src := '.text.n_system_$$_sysgetheapstatus$$theapstatus';  { 47 chars, len-1 = 46 = '.' }
  for i := 0 to length(src) - 1 do
    buf[i] := src[i + 1];
  buf[length(src)] := #0;
  p := @buf[0];
  s := string(p);
  writeln(s);
end.
