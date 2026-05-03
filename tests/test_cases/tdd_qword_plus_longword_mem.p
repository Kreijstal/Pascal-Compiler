program tdd_qword_plus_longword_mem;
{ Regression: KGPC was emitting `addq mem32, reg64` when adding a longword
  variable (32-bit, sitting in a 4-byte stack slot) to a qword variable.
  The CPU then read 8 bytes from a 4-byte memory location, picking up
  garbage (or an adjacent variable) in the upper 32 bits.
  This crashed FPC's pp.pas with internal error 200604031 because
  TObjSection.Datapos (PUInt = qword on x86_64) was being incorrectly
  computed via align_aword(v:aword;a:longword):aword. }

{$mode objfpc}

type
  AWord = qword;
  TObjSectionOfs = qword;

function align_aword(v:aword;a:longword):aword;
  begin
    if a>0 then
      a:=a-1;
    align_aword := (v+a) and aword(not aword(a));
  end;

var
  q : qword;
  l : longword;
  result : qword;
begin
  q := $40;
  l := 16;
  result := align_aword(q, l);
  writeln('aligned: ', result);

  q := $90;
  l := 16;
  result := align_aword(q, l);
  writeln('aligned: ', result);

  q := $f3;
  l := 8;
  result := align_aword(q, l);
  writeln('aligned: ', result);
end.
