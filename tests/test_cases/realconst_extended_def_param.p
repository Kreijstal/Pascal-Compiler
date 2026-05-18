program realconst_extended_def_param;
{$mode objfpc}

{ Regression test: a method/constructor that takes a stack-passed Extended
  value parameter together with a subsequent register-passed pointer
  parameter used to lose the pointer.  The Extended copy is performed via
  kgpc_move at function entry (callee side) and at the call site (caller
  side), and kgpc_move forwards to memmove which clobbers caller-saved
  registers including %rsi.  When the next parameter (def) was loaded into
  %rsi before the kgpc_move call ran, its value was destroyed before the
  callee could observe it.  Both the call-site emission and the callee
  prologue have been updated to spill / reorder so the pointer survives. }

type
  TFoo = class
    v: extended;
    d: pointer;
    constructor Create(val: extended; def: pointer);
  end;

constructor TFoo.Create(val: extended; def: pointer);
begin
  v := val;
  d := def;
end;

var
  f: TFoo;
  marker: longint;
begin
  marker := 12345;
  f := TFoo.Create(3.14, @marker);
  if f.d = @marker then writeln('ok') else writeln('FAIL');
end.
