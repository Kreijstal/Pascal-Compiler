program regr_shortstring_assign_dest_clobber;

{ Regression test: when result := f(obj.field.virtual_method) is generated for
  a shortstring return, the destination address register (used to point at
  the result buffer) must survive the RHS evaluation. Before the fix, codegen
  spilled the destination register but never reloaded it; the final
  kgpc_shortstring_to_shortstring then wrote into whatever heap address
  ended up in that register (typically a typesym field pointer), silently
  corrupting an unrelated allocation. The pp.pas bootstrap exercised this
  via generate_objectpascal_helper_key, where the corrupted def was later
  walked by add_helpers_and_generics and triggered a nil-VMT segfault. }

{$mode objfpc}

type
  TInner = class
    function Name: shortstring; virtual;
  end;

  TOuter = class
    inner: TInner;
  end;

function TInner.Name: shortstring;
begin
  Name := 'inner_name_value';
end;

{ make_key emulates make_mangledname: a shortstring-returning routine that
  takes two shortstring args, the second of which is obtained via a virtual
  method on a chained field reference. }
function make_key(const a, b: shortstring): shortstring;
begin
  make_key := a + '/' + b;
end;

function gen_key(obj: TOuter): shortstring;
begin
  { This is the exact pattern from generate_objectpascal_helper_key:
        result := f('', obj.field.virtual_method);
    The dest register for `result :=` must not be lost across the
    chained field access + virtual call evaluation. }
  gen_key := make_key('hi', obj.inner.Name);
end;

var
  o: TOuter;
  k: shortstring;
  i: longint;
begin
  o := TOuter.Create;
  o.inner := TInner.Create;
  for i := 1 to 4 do
  begin
    k := gen_key(o);
    writeln(k);
  end;
  o.inner.Free;
  o.Free;
end.
