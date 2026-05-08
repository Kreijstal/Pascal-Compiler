{ Regression for the second mismatched-allocator pair revealed when
  pp_bootstrap (KGPC building FPC pp.pas via --no-stdlib) crashed
  with "double free or corruption (out)" while compiling helloworld.p.

  Root cause:
    * KGPC initialises the FPC TMemoryManager dispatch table at C
      startup with libc malloc/free wrappers (kgpc_mm_*).
    * The FPC system unit's heap.inc declares MemoryManager as a typed
      const initialised to @SysGetMem / @SysFreeMem (FPC's arena heap).
    * That typed-const initialiser runs as part of FPC RTL initialisation
      and OVERWRITES the function pointers KGPC just installed.  After
      this point GetMem(...) goes through FPC's arena heap.
    * KGPC's Dispose() builtin used to lower to kgpc_dispose -> libc
      free directly, mismatching the FPC-arena allocator and corrupting
      the heap on the very first compilation.

  Fix: kgpc_new and kgpc_dispose route through MemoryManager so they
  always pair with whatever GetMem/FreeMem chose, regardless of which
  heap manager is active.  In KGPC stdlib mode MemoryManager stays
  bound to kgpc_mm_* (libc malloc/free), so behaviour there is
  byte-for-byte unchanged.

  This test simply exercises a New/Dispose pair under KGPC stdlib mode
  so a regression that breaks the libc fallback (e.g. by misreading
  MemoryManager offsets, or by failing to fall back when MemoryManager
  is uninitialised) trips at meson-test time.  The full FPC RTL bootstrap
  pin is the test_fpcrtl_pp_pas_bootstrap helloworld extension. }
program tdd_dispose_after_fpc_rtl_init;

type
  PRecord = ^TRecord;
  TRecord = record
    a, b, c: integer;
  end;

var
  p: PRecord;
  q: PRecord;

begin
  New(p);
  p^.a := 1;
  p^.b := 2;
  p^.c := 3;
  writeln('p=', p^.a, ' ', p^.b, ' ', p^.c);
  Dispose(p);

  { Allocate again; the previous Dispose must have actually returned the
    block to whichever allocator owns it, so this should succeed. }
  New(q);
  q^.a := 10;
  q^.b := 20;
  q^.c := 30;
  writeln('q=', q^.a, ' ', q^.b, ' ', q^.c);
  Dispose(q);

  writeln('OK');
end.
