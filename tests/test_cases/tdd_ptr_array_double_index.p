program tdd_ptr_array_double_index;
{ Regression: a pointer-to-array indexed with a comma double-index, p[i,j],
  must stride the first index by the whole pointee row and the SECOND index by
  the inner array's element size (NOT the pointee size again).  Mirrors FPC's
  tinterferencebitmap (rgobj.pas): fbitmap: ^array[byte] of P2, accessed
  fbitmap[i,j], and the destructor loop `if assigned(fbitmap[i,j]) then
  dispose(fbitmap[i,j])`.  KGPC scaled the second index by the row size (2048),
  reading far out of bounds; and assigned() over a comma-index added a stale
  extra dereference -> segfault in the register allocator during the FPC 3.2.2
  win64 system.pp bootstrap. }
{$mode objfpc}{$H-}
type
  T2 = array[byte] of set of byte;   { inner block }
  P2 = ^T2;
  T1 = array[byte] of P2;            { 256 pointers = one row }
  P1 = ^T1;
var
  fb: P1;
  raw: ppointer;
  i, j: byte;
  freed: integer;
begin
  { --- addressing: write via flat pointer math, read via the comma form --- }
  fb := AllocMem(sizeof(T1) * 2);
  raw := ppointer(fb);
  raw[0 * 256 + 0] := pointer(ptruint($1111));
  raw[0 * 256 + 7] := pointer(ptruint($2222));
  raw[1 * 256 + 5] := pointer(ptruint($3333));
  writeln(ptruint(fb[0, 0]));
  writeln(ptruint(fb[0, 7]));
  writeln(ptruint(fb[1, 5]));
  writeln(ptruint(fb[1, 0]));
  writeln(assigned(fb[1, 5]));
  writeln(assigned(fb[1, 4]));
  freemem(fb);

  { --- destructor pattern: new / assigned / dispose over the comma form --- }
  fb := AllocMem(sizeof(T1) * 2);
  new(fb[0, 0]);
  new(fb[1, 5]);
  freed := 0;
  for i := 0 to 1 do
    for j := 0 to 255 do
      if assigned(fb[i, j]) then
      begin
        dispose(fb[i, j]);
        inc(freed);
      end;
  freemem(fb);
  writeln('freed=', freed);
end.
