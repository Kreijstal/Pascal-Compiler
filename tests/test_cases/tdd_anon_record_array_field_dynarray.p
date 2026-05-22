{ Regression test for KGPC codegen bug exposed by FPC pp.pas bootstrap.

  pp.pas's unicodedata.IncrementalCompareString_NonIgnorable declares
  TComputeKeyContext, which mixes a dynamic-array field
  ("removedCharIndex : array of DWord") with a fixed-array-of-anonymous-record
  field ("locHistory : array[0..24] of record ... end").

  When codegen emits the function epilogue's managed-dynarray cleanup, it
  walks every field of the local record looking for nested dynarrays.  For
  array-typed fields it calls codegen_sizeof_type to compute the element
  size.  Two call sites passed NULL for record_type and only the type_id;
  when the element is an anonymous record, type_id is also NULL and
  codegen_sizeof_type reported "Unable to resolve anonymous record type for
  size computation."

  The error wasn't fatal per se, but ctx->had_error stayed set across
  subsequent subprograms (the non-cache-miss path in codegen_subprograms
  didn't save/restore it), which caused unrelated downstream codegen calls
  to short-circuit and the compiler ultimately aborted with
  "gencode_leaf_var: Unsupported expr type" while compiling
  trealconstnode.pass_typecheck.

  The fix passes field->array_element_record (the in-memory layout) into
  codegen_sizeof_type from both call sites in codegen_emit_dynarray_cleanup_record_fields,
  and saves/restores ctx->had_error around each subprogram's codegen in the
  non-cache-miss path of codegen_subprograms.

  This minimal program exercises both halves: dynarray cleanup walks an
  outer record whose layout contains a fixed array of an anonymous record. }
program tdd_anon_record_array_field_dynarray;

type
  TKeyContext = record
    payload      : array of longint;          { managed dynarray }
    locHistory   : array[0..3] of record       { fixed array of anon record }
                                   i  : longint;
                                   cl : pointer;
                                 end;
  end;

procedure RunOnce;
var
  ctx : TKeyContext;
begin
  SetLength(ctx.payload, 4);
  ctx.payload[0] := 11;
  ctx.locHistory[0].i := 42;
  ctx.locHistory[0].cl := nil;
  Writeln(ctx.payload[0], ' ', ctx.locHistory[0].i);
end;

begin
  RunOnce;
  Writeln('ok');
end.
