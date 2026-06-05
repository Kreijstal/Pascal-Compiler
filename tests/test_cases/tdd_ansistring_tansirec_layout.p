program tdd_ansistring_tansirec_layout;
{$mode objfpc}{$H+}
{ KGPC's runtime AnsiString header must match FPC's 64-bit TAnsiRec layout so
  that KGPC-compiled FPC RTL source (which reads PAnsiRec(S-AnsiFirstOff)^
  directly, with AnsiFirstOff = SizeOf(TAnsiRec) = 24) sees the right fields:

      CodePage    : Word    @ data-24
      ElementSize : Word    @ data-22
      Dummy       : DWord   @ data-20
      Ref         : SizeInt @ data-16
      Len         : SizeInt @ data-8

  With KGPC's earlier 16-byte header, ElementSize (data-22) and Ref (data-16)
  fell before/inside the wrong words, and FPC startup's SetCodePage wrote the
  CodePage 8 bytes ahead of the allocation, corrupting the heap ("malloc():
  corrupted size vs prev_size") while booting the bootstrapped 3.2.2 compiler.
  Len lives at data-8 in either layout, so ElementSize is the discriminator:
  reading 1 there proves the 24-byte layout. }
type
  PAnsiRec = ^TAnsiRec;
  TAnsiRec = packed record
    CodePage    : Word;
    ElementSize : Word;
    Dummy       : DWord;
    Ref         : PtrInt;
    Len         : PtrInt;
  end;
var
  s, t : ansistring;
  rec  : PAnsiRec;
begin
  t := 'hel';
  s := t + 'lo';                                     { runtime-built heap ansistring }
  rec := PAnsiRec(PtrUint(@s[1]) - SizeOf(TAnsiRec));
  writeln(rec^.ElementSize);   { 1 - AnsiChar element size at data-22 }
  writeln(rec^.Len);           { 5 - length at data-8 }
  writeln(s);                  { hello }
end.
