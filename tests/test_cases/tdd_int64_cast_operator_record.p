program tdd_int64_cast_operator_record;
{ Regression test for the textrec.inc "Data element too large" miscompile
  in pp_bootstrap (system.pp), root-caused to FPC's pexpr.pas line 137:
    def := cstringdef.createshort(int64(tordconstnode(p).value), true);

  Tconstexprint is a 16-byte record with an `operator := (const c: Tconstexprint): int64`
  conversion overload.  Without this fix, KGPC stripped the `int64()` typecast in
  build_expr_tree and emitted the raw record address as the byte argument to
  createshort, so `len` was set to garbage.  That cascaded into every
  subsequent record field offset (textrec, etc.) becoming negative, and the
  symtable.pas:1377 size check triggered "Data element too large" for every
  TextRec field at offsets >= textrec.inc:57.

  Mirrors the constexp.pas pattern (overflow:boolean; signed:boolean;
  case uvalue:qword; svalue:int64) and the explicit int64() typecast at
  the call site. }

type
  TConstExpr = record
    overflow: boolean;
    case signed: boolean of
      false: (uvalue: qword);
      true:  (svalue: int64);
  end;

operator := (const c: TConstExpr): int64;
begin
  result := c.svalue;
end;

procedure CreateShort(l: byte; flag: boolean);
begin
  if l = 3 then
    writeln('OK explicit int64() cast: l=', l)
  else
    writeln('FAIL explicit int64() cast: l=', l, ' expected 3');
end;

procedure CreateImplicit(l: byte; flag: boolean);
begin
  if l = 5 then
    writeln('OK implicit conversion: l=', l)
  else
    writeln('FAIL implicit conversion: l=', l, ' expected 5');
end;

var
  ce: TConstExpr;
begin
  { Reproduces FPC's pexpr.pas line 137: createshort(int64(value), true) }
  ce.signed := true;
  ce.svalue := 3;
  CreateShort(int64(ce), true);

  { Implicit conversion (no explicit cast) — already worked before the fix }
  ce.svalue := 5;
  CreateImplicit(ce, true);
end.
