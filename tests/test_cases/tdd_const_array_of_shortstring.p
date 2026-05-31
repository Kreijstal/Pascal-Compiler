program tdd_const_array_of_shortstring;
{ Regression: a typed constant array of ShortString (string[N]) must store a
  proper Pascal ShortString in each element (length byte at offset 0, chars at
  offsets 1..N).  KGPC formerly initialized each element as a plain char array,
  writing the first character into the length-byte slot and shifting the text
  left by one, so Length()/High() and content were all wrong.  This broke FPC's
  raatt.pas is_asmdirective table (array[...] of string[10]) and thus any
  attempt to compile the RTL's GAS inline assembly. }
type
  tkw = string[10];
const
  dirs : array[0..3] of tkw = ('.byte', '.balign', '.p2align', '.long');
var
  i : longint;
  hs : string;
  hits : longint;
begin
  for i := 0 to 3 do
    writeln(i, ' len=', Length(dirs[i]), ' "', dirs[i], '"');

  { the actual is_asmdirective pattern: compare a (lowered) string against the
    fixed-capacity ShortString table entries. }
  hits := 0;
  hs := '.balign';
  for i := 0 to 3 do
    if hs = dirs[i] then
      Inc(hits);
  writeln('match .balign: ', hits);

  hs := '.nope';
  hits := 0;
  for i := 0 to 3 do
    if hs = dirs[i] then
      Inc(hits);
  writeln('match .nope: ', hits);
end.
