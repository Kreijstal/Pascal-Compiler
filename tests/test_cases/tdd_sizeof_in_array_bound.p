{ Regression: an inline array variable whose bound is a constant expression
  containing sizeof(record) (e.g. array[0..sizeof(trec)-1] of byte) used to
  collapse to length 1.  Array-type bounds are resolved at parse time, before
  the record layout is cached, so kgpc_type_sizeof(record) returned -1 there
  and the bound failed, leaving the default 0..0 range.  The inline-array var
  path in semcheck consumed those failed 0/0 bounds without re-resolving the
  stored dimension strings via the symtab-aware sizeof evaluator.  This was the
  wall blocking the KGPC->FPC Windows self-host fixpoint.  Covers inline var,
  named alias, record field, and a non-trivial const expression (*2-1). }
program tdd_sizeof_in_array_bound;
type
  trec = packed record
    a: longword; b: longword; c: longword;
    section: smallint; empty: word; typ: byte; aux: byte;
  end;
  tarr = array[0..sizeof(trec)-1] of byte;
  twrap = record
    data: array[0..sizeof(trec)-1] of byte;
  end;
var
  buf   : array[0..sizeof(trec)-1] of byte;
  named : tarr;
  wrap  : twrap;
  dbl   : array[0..sizeof(trec)*2-1] of byte;
  ok    : boolean;
begin
  ok := true;
  if SizeOf(trec) <> 18 then ok := false;
  if SizeOf(buf) <> 18 then ok := false;
  if High(buf) <> 17 then ok := false;
  if SizeOf(named) <> 18 then ok := false;
  if High(named) <> 17 then ok := false;
  if SizeOf(wrap.data) <> 18 then ok := false;
  if High(wrap.data) <> 17 then ok := false;
  if SizeOf(dbl) <> 36 then ok := false;
  if High(dbl) <> 35 then ok := false;
  if ok then WriteLn('OK')
  else WriteLn('FAIL');
end.
