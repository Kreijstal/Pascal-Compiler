{ Regression test for KGPC bug: passing a ShortString-typed by-value
  argument whose value is computed by an AnsiString concatenation that
  references global storage (typed const, global var).  The codegen for
  SHORTSTRING_TYPE arguments allocated a stack buffer and stored its
  address in a register, then evaluated the AnsiString expression -- but
  the inner expression uses scratch registers, including the buf_addr_reg
  itself when it gets reloaded with a leaq of a global symbol address
  (e.g. a typed-const array base).  The codegen then called
  kgpc_string_to_shortstring with the now-stale register value, writing
  the resulting ShortString into global storage instead of the stack
  buffer.  In FPC's pp.pas this manifested as
  TAsmLabel.create_non_global producing labels with empty Name fields
  because the TSymStr passed to TFPHashObject.Create was the empty
  ShortString left at the buffer slot, and the actual computed name
  ended up being written into the asmlabeltypeprefix typed-const array.
  Symptom: "Asm: Duplicate label" at every label.

  See KGPC/CodeGenerator/Intel_x86-64/codegen_expression.c, around the
  SHORTSTRING_TYPE arg materialization (kgpc_string_to_shortstring call). }
program tdd_shortstring_param_concat_typed_const_array;

var
  globalA : string;
  globalB : string;

function nrstr(v: longint): string;
var
  s: string;
begin
  Str(v, s);
  nrstr := s;
end;

procedure echo(s: ShortString);
begin
  writeln(s);
end;

procedure check(prefix: ShortString; nr: longint);
begin
  { This passes a complex AnsiString expression as a ShortString-by-value
    parameter.  The expression uses globals, so its evaluation requires
    leaq instructions targeting global symbols, which previously
    clobbered the ShortString-arg buf_addr_reg. }
  echo(prefix + globalA + globalB + nrstr(nr));
end;

begin
  globalA := 'aa';
  globalB := 'bb';
  check('.L', 10);
  check('.L', 20);
  check('.L', 33);
end.
