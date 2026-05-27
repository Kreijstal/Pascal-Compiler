program OrdInSetTypedConst;
const
  Vowels: set of Byte = [Ord('a'), Ord('e'), Ord('i'), Ord('o'), Ord('u')];
  AlphaRange: set of Byte = [Ord('a')..Ord('z'), Ord('A')..Ord('Z'), Ord('-'), Ord('_')];
var
  i: integer;
  count: integer;
begin
  count := 0;
  for i := 0 to 255 do
    if i in Vowels then count := count + 1;
  if count <> 5 then begin writeln('FAIL vowels=', count); halt(1); end;

  count := 0;
  for i := 0 to 255 do
    if i in AlphaRange then count := count + 1;
  if count <> 54 then begin writeln('FAIL alpha=', count); halt(2); end;

  if not (Ord('-') in AlphaRange) then begin writeln('FAIL dash'); halt(3); end;
  if Ord('!') in AlphaRange then begin writeln('FAIL bang'); halt(4); end;

  writeln('OK');
end.
