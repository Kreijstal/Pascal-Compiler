{$mode objfpc}
program tdd_pp_bootstrap_constexprint_overload_arg;

{ Regression: pp.pas Windows-bootstrap was emitting hundreds of spurious
  "Inc increment must be an integer" / "call ... does not match any
  available overload" errors.

  Root cause was unrelated to Tconstexprint per se: the typecast
  reinterpreter in SemCheck_Expr_Types.c happily promoted a parser-rewritten
  method call (`obj.size`) into a typecast `TSize(obj)` whenever the method
  name happened to coincide with a global type identifier reachable in the
  current scope.

  On the Windows-target compile of pp.pas, `rtl/win/wininc/struct.inc`
  introduces `SIZE = TSize` (the GDI rectangle/size record).  After that,
  every `sizesinttype.size` / `result.ofs := ...; inc(..., somedef.size)`
  call inside `compiler/aasmcnst.pas`, `compiler/ogbase.pas`, etc. got
  rewritten as `TSize(somedef)`, which then failed every subsequent type
  check.

  Fix: skip typecast reinterpretation when the function-call expression is
  flagged `is_method_call_placeholder`, i.e. came from `expr.id` syntax
  with an explicit receiver.

  This test mimics the failing pattern: a record method named `size` whose
  name clashes with a `SIZE` type alias also visible at the call site. }

type
  TSize = record
    cx, cy: longint;
  end;
  SIZE = TSize;

  TIntDef = record
    val: int64;
    function size: int64;
  end;

function TIntDef.size: int64;
begin
  result := val;
end;

var
  sizesinttype: TIntDef;
  ofs: int64;
begin
  sizesinttype.val := 4;
  ofs := 10;
  { Before the fix this raised "Inc increment must be an integer" because
    sizesinttype.size was reinterpreted as the typecast TSize(sizesinttype). }
  inc(ofs, sizesinttype.size);
  writeln(ofs);
end.
