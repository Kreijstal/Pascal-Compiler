program nested_func_returns_class;

{ Regression: a nested function returning a class instance (an 8-byte
  pointer) must not have its result truncated to 32 bits in the
  return-value load.  The return-size lookup keys off the function symbol
  in the global symtab, which a nested function is absent from, so the
  class return type formerly defaulted to a 4-byte (movl) load and the
  heap pointer was truncated -> dangling -> crash.  Surfaced on Win64
  (high heap addresses make the truncation fatal); benign-looking on
  Linux only because low addresses survive 32 bits.  Calling a method on
  the returned instance both exercises the pointer and proves it round-
  tripped intact. }

type
  tval = class
    x: longint;
    constructor create(a: longint);
    function get: longint;
  end;

  constructor tval.create(a: longint);
  begin
    x := a;
  end;

  function tval.get: longint;
  begin
    get := x;
  end;

  function outer: tval;
    function inner: tval;
    begin
      inner := tval.create(42);
    end;
  begin
    outer := inner;
  end;

var
  v: tval;
begin
  v := outer;
  writeln(v.get);
  v.free;
end.
