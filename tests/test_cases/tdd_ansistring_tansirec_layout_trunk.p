program tdd_ansistring_tansirec_layout_trunk;
{$mode objfpc}{$H+}
{ Companion to tdd_ansistring_tansirec_layout.p, for FPC *trunk* (3.3.1), whose
  64-bit TAnsiRec is 16 bytes (no Dummy, Ref:Longint):

      CodePage    : Word    @ data-16
      ElementSize : Word    @ data-14
      Ref         : Longint @ data-12
      Len         : SizeInt @ data-8

  KGPC must NOT hardcode the 24-byte (3.2.2) header: it reads SizeOf(TAnsiRec)
  and the Ref/Len geometry from the RTL it is compiling and lays its runtime
  managed-string header out to match.  This program declares the trunk 16-byte
  TAnsiRec, so the runtime must allocate a 16-byte header here and the negative
  offsets below must line up.  Len lives at data-8 in either layout, so reading
  ElementSize at data-14 (== data-SizeOf(TAnsiRec)+2) is the discriminator:
  reading 1 there proves the 16-byte layout was used. }
type
  PAnsiRec = ^TAnsiRec;
  TAnsiRec = packed record
    CodePage    : Word;
    ElementSize : Word;
    Ref         : Longint;
    Len         : SizeInt;
  end;
var
  s, t : ansistring;
  rec  : PAnsiRec;
begin
  t := 'hel';
  s := t + 'lo';                                     { runtime-built heap ansistring }
  rec := PAnsiRec(PtrUint(@s[1]) - SizeOf(TAnsiRec));
  writeln(rec^.ElementSize);   { 1 - AnsiChar element size at data-14 }
  writeln(rec^.Len);           { 5 - length at data-8 }
  writeln(s);                  { hello }
end.
