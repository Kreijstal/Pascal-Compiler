{ Regression: invoking a constructor on an existing instance value
  (`Inst.Create(...)`, not `TClass.Create(...)`) must run the constructor body
  as an ordinary method with Self = the instance -- it must NOT allocate a new
  object.

  KGPC previously emitted the allocating path for instance-qualified
  constructor calls, reading the instance's first qword (its VMT pointer) as
  the allocation size.  For a local-variable receiver the size happened to be a
  small constant so it merely leaked a new object (and left the original
  unchanged); for an array-element receiver of class type -- exactly FPC's
  `aoptobj.pas` TransferUsedRegs `dest[i].Create_Regset(...)` -- it passed the
  VMT *address* (~35 MB) as the size, so every peephole pass did a multi-MB
  AllocMem+zero.  That made the KGPC-built FPC compiler (pp_bootstrap)
  pathologically slow under -O1/-O2 (the assembler peephole optimiser), which
  blocked the optimised FPC self-build.

  Expected: f.Create resets the SAME instance, so x becomes 2 (no realloc). }
program constructor_called_on_instance_no_realloc;
{$mode objfpc}
type
  TFoo = class
    x: longint;
    constructor Create(a: longint);
  end;
  TArr = array[0..2] of TFoo;
constructor TFoo.Create(a: longint);
begin
  x := a;
end;
var
  f: TFoo;
  arr: TArr;
  i: longint;
  p: pointer;
begin
  f := TFoo.Create(1);          { class-ref ctor: allocates }
  p := pointer(f);
  f.Create(2);                  { instance ctor-as-method: must NOT reallocate }
  if pointer(f) <> p then
    writeln('FAIL: reallocated')
  else
    writeln('x=', f.x);         { expect x=2 on the same object }

  { array-element receiver, mirroring TransferUsedRegs dest[i].Create_Regset }
  for i := 0 to 2 do
    arr[i] := TFoo.Create(i);
  for i := 0 to 2 do
    arr[i].Create(i + 10);      { re-init each existing element, no realloc }
  writeln('arr=', arr[0].x, ',', arr[1].x, ',', arr[2].x);
end.
