{ Regression: a procedure-scope typed const declared as
    array[enum_type, enum_type] of byte-enum
  must be allocated rows*cols bytes, not just rows.

  Before the fix, codegen.c only consulted kgpc_type_get_array_dimension_info
  on the local symbol's KgpcType, which for procedure-scope typed consts only
  modelled the outer dimension (dim_count==1). The fallback that walked
  arr->array_dimensions parsed only numeric "lo..hi" range strings and treated
  named enum/range tokens like "tbasedef" as unparseable -- so it skipped the
  expansion. The .comm under-allocated to (outer_len * element_size) bytes
  and runtime init writes ran off into adjacent BSS, corrupting unrelated
  typed-const arrays (manifested as FPC IE 2014041302 in pp_bootstrap because
  defcmp.pas's basedefconvertsexplicit overran cpubase.pas's flags_2_cond
  init-guard byte). }
program local_typed_const_enum_indexed_2d;
type
  tbasedef = (bvoid, bchar, bint, bbool);
  tconverttype = (tc_none, tc_a, tc_b, tc_c);

procedure go;
const
  m : array[tbasedef, tbasedef] of tconverttype =
    ((tc_none, tc_none, tc_none, tc_none),
     (tc_none, tc_a,    tc_none, tc_none),
     (tc_none, tc_none, tc_b,    tc_none),
     (tc_none, tc_none, tc_none, tc_c));
begin
  writeln(Ord(m[bvoid][bvoid]));
  writeln(Ord(m[bchar][bchar]));
  writeln(Ord(m[bint][bint]));
  writeln(Ord(m[bbool][bbool]));
  writeln(Ord(m[bint][bchar]));
end;

begin
  go;
end.
