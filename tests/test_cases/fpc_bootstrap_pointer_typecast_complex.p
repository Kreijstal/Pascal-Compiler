program fpc_bootstrap_pointer_typecast_complex;

{$mode objfpc}

type
  PByte = ^Byte;
  PWord = ^Word;

var
  Buf: array[0..3] of Byte;
  P: Pointer;
  W: Word;

begin
  Buf[0] := 1;
  Buf[1] := 2;
  Buf[2] := 3;
  Buf[3] := 4;
  P := @Buf[0];

  writeln('b2=', PByte(PtrUInt(P) + 2)^);
  W := PWord(Pointer(PtrUInt(P) + 1))^;
  writeln('w1=', W);
end.
