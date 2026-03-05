program tdd_unaligned_record_typecast;

type
  TRec = packed record
    a, b, c: byte;
  end;
  PRec = ^TRec;

var
  src: TRec;
  dst: TRec;
  p: PRec;

begin
  src.a := 7;
  src.b := 8;
  src.c := 9;
  p := @src;
  dst := Unaligned(p^);
  if (dst.a = 7) and (dst.b = 8) and (dst.c = 9) then
    writeln(1)
  else
    writeln(0);
end.
