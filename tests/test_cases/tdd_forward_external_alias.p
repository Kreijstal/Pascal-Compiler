{ Regression test: a function declared `forward;` and later given an
  `external name 'X';` implementation must have callers emit `call X`,
  not `call <pascal_mangled_name>`.

  Without this fix, codegen would emit the Pascal-mangled name of the
  forward decl and the link would fail with an undefined reference.

  This mirrors FPC's RTL pattern in dynarrh.inc/dynarr.inc:
    function DynArraySize(a: pointer): tdynarrayindex; forward;
    function DynArraySize(a: pointer): tdynarrayindex;
      external name 'FPC_DYNARRAY_LENGTH';
  where callers within the same unit need to bind to the alias. }
program tdd_forward_external_alias;

{ Forward declaration of a procedure (no body). }
procedure ReinitStdio; forward;

{ Caller that resolves ReinitStdio through the forward decl.
  Without the fix, this emits `call <mangled ReinitStdio>` and the
  link fails with an undefined reference. }
procedure DoIt;
begin
  ReinitStdio;
end;

{ External alias supplied later in the unit.  Binds to a real runtime
  symbol so the link succeeds. }
procedure ReinitStdio;
  external name 'kgpc_reinit_stdio';

begin
  DoIt;
  Writeln('ok');
end.
