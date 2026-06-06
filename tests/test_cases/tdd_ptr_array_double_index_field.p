program tdd_ptr_array_double_index_field;
{ Regression: the comma-double-index pointer-to-array stride fix must also work
  when the pointer base is a CLASS FIELD reached via implicit Self (fb[i,j] ==
  Self.fb[i,j]), not just a plain global variable.  This is the exact shape of
  FPC's tinterferencebitmap (rgobj.pas): fbitmap is a field, and its destructor
  runs `if assigned(fbitmap[i,j]) then dispose(fbitmap[i,j])`.  The element type
  recovery prefers resolved_kgpc_type, so the field/Self form must resolve the
  pointee array the same way the global form does (otherwise j re-uses the row
  size and reads far out of bounds -> the register-allocator segfault). }
{$mode objfpc}{$H-}
type
  T2 = array[byte] of set of byte;   { inner block }
  P2 = ^T2;
  T1 = array[byte] of P2;            { 256 pointers = one row }
  P1 = ^T1;
  TIB = class
    fb: P1;
    constructor Create;
    destructor Destroy; override;
  end;
constructor TIB.Create;
begin
  fb := AllocMem(sizeof(T1) * 2);
  new(fb[0, 0]);     { field access via implicit Self, comma double-index }
  new(fb[1, 5]);
end;
destructor TIB.Destroy;
var i, j: byte; freed: integer;
begin
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
end;
var o: TIB;
begin
  o := TIB.Create;
  o.Destroy;
end.
