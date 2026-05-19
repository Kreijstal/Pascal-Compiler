{ Regression test: PtrUint(@PType(nil)^.field) must yield the field's offset,
  not 0.  KGPC's codegen for the inline-typecast-of-nil-then-dereference-then-
  field-access pattern was losing the field offset, so FPC's pp_bootstrap
  would compute GetMem(PtrUint(@PMemoryRegionNode(nil)^.data) + alloc) as
  GetMem(alloc) — undersizing TViHashList.FShortstringRegion's buffer by
  16 bytes (sizeof(TMemoryRegionNode without data)).  That overflow
  corrupted the heap's CommonHeader for adjacent allocations, so the
  resulting pp_stage2 crashed in fpc_ansistr_decr_ref's freemem path
  (under SysFreeMem at $428274) on essentially every program — the
  Replace path during WriteLogo's "$FPCVERSION" substitution was the
  first trigger.

  The fix must restore the field offset in the address computation
  emitted for `@PType(nil)^.field`, both for direct assignment to a
  pointer and for explicit PtrUint(...) ordinal typecast (which simplify
  folds via tpointerconstnode -> tordconstnode in FPC). }
{$mode objfpc}{$H+}
program tdd_pp_stage2_field_offset_nil_typecast;

type
  TNode = record
    n, alloc : uint32;
    next : pointer;
    data : byte;
  end;
  PNode = ^TNode;
var
  u : ptruint;
  p : pointer;
begin
  { Reference path: an address taken through a real variable already works
    in pp_bootstrap; this anchors the expected offset for comparison. }
  u := PtrUint(@PNode(nil)^.data);
  p := @PNode(nil)^.data;
  writeln('inline-cast=', u, ' assign=', PtrUint(p));
end.
