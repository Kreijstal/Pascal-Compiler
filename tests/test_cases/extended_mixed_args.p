program ExtendedMixedArgs;

function Mix(a: LongInt; b: Extended; c: Double): LongInt;
begin
  Mix := a + Trunc(b) + Trunc(c);
end;

begin
  WriteLn(Mix(5, 2.75, 1.5));
end.
