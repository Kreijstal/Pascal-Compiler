program with_field_shadows_local;

{ Regression: inside `with rec do`, an identifier that names a field of the
  WITH record must resolve to that field, not to an enclosing local variable
  of the same name.  KGPC formerly preferred a current-scope local over the
  WITH field, so `with paraloc^ do if loc = LOC_REFERENCE then
  inc(reference.offset, first_parm_offset)` in FPC's cpupara.pas read the
  same-named local `loc` array instead of `paraloc^.loc`; the comparison
  never matched and the +16 first_parm_offset adjustment for the first
  stack-passed parameter was dropped.  That miscompiled every function's
  first stack parameter (read at [rbp+0] instead of [rbp+16]) and killed
  the self-hosted pp_stage2 at startup (RTE 203, the 7-arg mmap syscall).
  Both a value read (the `if loc = 18`) and a write through a variant field
  (`inc(offset, 16)`) inside the WITH are exercised here. }

type
  PNode = ^TNode;
  TNode = record
    next: PNode;
    loc: longint;
    case longint of
      0: (offset: int64);
      1: (reg: longint);
  end;

var
  head, p: PNode;
  a, b: TNode;
  loc: longint;  { same name as the record field; must NOT shadow it }

begin
  loc := 999;  { the outer local is never 18, so if it shadowed, no inc runs }
  a.next := @b; a.loc := 18; a.offset := 100;
  b.next := nil; b.loc := 18; b.offset := 200;
  head := @a;
  p := head;
  while p <> nil do
  begin
    with p^ do
      if loc = 18 then
        inc(offset, 16);
    p := p^.next;
  end;
  writeln(a.offset);
  writeln(b.offset);
  writeln(loc);
end.
