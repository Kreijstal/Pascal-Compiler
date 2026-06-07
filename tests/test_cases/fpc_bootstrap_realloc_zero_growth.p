program fpc_bootstrap_realloc_zero_growth;
{$mode objfpc}

type
  PWords = ^TWords;
  TWords = array[0..63] of word;

var
  p, poison: Pointer;
  words: PWords;
  i: integer;
  ok: boolean;

begin
  GetMem(poison, 32);
  words := PWords(poison);
  for i := 0 to 15 do
    words^[i] := $ffff;
  FreeMem(poison);

  p := nil;
  ReallocMem(p, 32);
  words := PWords(p);
  ok := true;
  for i := 0 to 15 do
    if words^[i] <> 0 then
      ok := false;

  words^[0] := 123;
  ReallocMem(p, 96);
  words := PWords(p);
  if words^[0] <> 123 then
    ok := false;
  for i := 16 to 47 do
    if words^[i] <> 0 then
      ok := false;

  FreeMem(p);
  if ok then
    writeln('ok')
  else
    writeln('bad');
end.
