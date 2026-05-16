{ Regression test: Pascal set subset operator (<=) and superset (>=)
  must do bitwise inclusion, not integer comparison.

  Bug: KGPC's expr_tree relop codegen emitted "cmpl / setbe" — an
  unsigned integer compare — for set <=, so disjoint or overlapping
  sets like {bit 1, bit 5} <= {bit 7} evaluated as 34<=128 (TRUE)
  instead of false (TRUE is not a subset of {bit 7}).

  In FPC pp.pas, this miscompiled tabstractprocdef.no_self_node:
      Result := [po_staticmethod, po_classmethod] <= procoptions ...
  When procoptions contained any high-bit flag (e.g. po_methodpointer
  with ordinal 7 = bit 128) the integer compare made no_self_node
  return TRUE for ordinary instance methods, so pp_bootstrap then
  refused all unqualified field accesses inside method bodies
  with "Identifier not found self" / "Pointer to object expected". }
program set_subset_le;
{$mode objfpc}
type
  topt = (
    o00,o01,o02,o03,o04,o05,o06,o07,o08,o09,
    o10,o11,o12,o13,o14,o15,o16,o17,o18,o19,
    o20,o21,o22,o23,o24,o25,o26,o27,o28,o29,
    o30,o31,o32,o33,o34,o35,o36,o37,o38,o39,
    o40,o41,o42,o43,o44,o45,o46,o47,o48,o49,
    o50,o51,o52,o53,o54,o55,o56,o57,o58,o59,
    o60,o61,o62,o63,o64,o65,o66,o67,o68,o69,
    o70,o71,o72,o73,o74,o75,o76,o77,o78,o79
  );
  topts = set of topt;

  tsmall = (s0, s1, s2, s3, s4, s5, s6, s7);
  tsmalls = set of tsmall;

function small_subset(opts: tsmalls): boolean;
begin
  result := [s1, s5] <= opts;
end;

function large_subset(opts: topts): boolean;
begin
  result := [o01, o05] <= opts;
end;

function large_superset(opts: topts): boolean;
begin
  result := opts >= [o01, o05];
end;

begin
  { small set subset checks }
  writeln(small_subset([s0, s6]));   { FALSE — no overlap }
  writeln(small_subset([s1, s5]));   { TRUE  — equal }
  writeln(small_subset([s1, s5, s7]));{ TRUE  — superset }
  writeln(small_subset([s7]));       { FALSE — high bit only }
  writeln(small_subset([]));         { FALSE — empty }
  { small literal LHS, large variable RHS }
  writeln(large_subset([o00, o06])); { FALSE }
  writeln(large_subset([o01, o05])); { TRUE  }
  writeln(large_subset([o07]));      { FALSE — bug case: 34<=128 if buggy }
  writeln(large_subset([o40, o50])); { FALSE — RHS high bits, LHS low bits }
  writeln(large_subset([]));         { FALSE }
  { same comparison via >= (RHS supersets LHS) }
  writeln(large_superset([o00, o06]));{ FALSE }
  writeln(large_superset([o01, o05]));{ TRUE  }
  writeln(large_superset([o07]));    { FALSE }
end.
