program tdd_as_cast_field_access;
{ Regression: reading or writing a field through an "as" typecast on an
  addressable class expression must dereference the storage exactly once.

  codegen_address_for_expr(EXPR_AS) used to return the already-dereferenced
  instance pointer: the runtime cast-check helper clobbers the register it is
  handed (loading the instance pointer out of the slot), and the EXPR_AS case
  returned that clobbered register as if it were the slot address.  Record-field
  access then dereferenced it a second time, reading/writing through the
  object's VMT instead of the object.

  In FPC's rax86 this corrupted (operands[i] as tx86operand).vopext reads,
  yielding a constant garbage value (a smallint pulled out of the VMT) and
  decorating plain SSE moves with bogus AVX-512 EVEX masks, which broke
  compiling the RTL's GAS inline assembly. }
type
  TBase = class
    tag : longint;
  end;
  TDer = class(TBase)
    a : smallint;
    b : longint;
    constructor Create;
  end;

constructor TDer.Create;
begin
  inherited Create;
  a := 12345;
  b := 678;
end;

var
  arr : array[1..3] of TBase;
  o   : TBase;
  i   : longint;
begin
  for i := 1 to 3 do
    arr[i] := TDer.Create;

  { Read a field through an "as" cast on an array element (addressable). }
  for i := 1 to 3 do
    writeln('read i=', i, ' a=', (arr[i] as TDer).a, ' b=', (arr[i] as TDer).b);

  { Write a field through an "as" cast, then read it back via a hard cast. }
  o := arr[2];
  (o as TDer).a := 999;
  (o as TDer).b := -4;
  writeln('write a=', TDer(o).a, ' b=', TDer(o).b);
end.
