program tdd_move_pointer_array_shift;

{ Repro for kgpc miscompile of Move(src, dst, count) when both src and dst
  are array elements whose ELEMENT type is Pointer.  In FPC RTL mode the
  bug: kgpc's codegen_builtin_move dispatched through codegen_pass_arguments
  with procedure_name="Move", which re-fetched FPC's formal arg list
  (const source; var dest; count) and matched it positionally against the
  swapped C-ABI args [dst, src, count].  The const-untyped pointer-by-value
  optimization then fired for the dst slot and passed the dst array
  element's VALUE (a pointer) instead of its address — memmove wrote
  through that arbitrary pointer, corrupting unrelated memory.

  Manifested in pp_bootstrap as 0xa400000001 freelist corruption from
  TFPList.Delete (lists.inc:148 calls Move to shift elements down).

  This test exercises the same call shape — Move on a Pointer array — and
  asserts the shift produces the correct values rather than crashing or
  corrupting memory.  Pure-KGPC mode uses var/var formals so it doesn't
  hit the heuristic; the regression coverage for the FPC RTL path comes
  from the FPC RTL test suite. }

var
  buf: array[0..3] of Pointer;
  i: Integer;
begin
  buf[0] := Pointer(PtrUInt(11));
  buf[1] := Pointer(PtrUInt(22));
  buf[2] := Pointer(PtrUInt(33));
  buf[3] := nil;

  { Shift elements [1..3] down by one — equivalent to TFPList.Delete(0). }
  Move(buf[1], buf[0], 3 * SizeOf(Pointer));

  for i := 0 to 3 do
    Writeln(PtrUInt(buf[i]));
end.
