{ Regression test for KGPC bug: sibling nested function calls were
  miscompiled because the caller dropped the parent frame pointer
  (static link) when calling a sibling that accesses outer scope.

  Setup: outer has two sibling nested functions, helper and inner.
  - inner accesses outer's local `shared` (needs static link to receive).
  - helper does NOT access outer vars directly, but calls inner.
    Before the fix, helper neither received nor forwarded a static link;
    inner's access to `shared` then either crashed (NULL deref) or
    corrupted unrelated memory. }
program sibling_nested_static_link;

procedure outer;
var
  shared: longint;

  function inner(flag: boolean): boolean; forward;

  function helper(flag: boolean): boolean;
  begin
    helper := inner(flag);
  end;

  function inner(flag: boolean): boolean;
  begin
    shared := shared + 1;
    inner := flag;
  end;

begin
  shared := 0;
  if helper(true) then
    writeln('OK: shared=', shared)
  else
    writeln('FAIL: shared=', shared);
end;

begin
  outer;
end.
