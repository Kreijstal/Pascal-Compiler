{ Reduced reproducer for the pp_bootstrap "double free or corruption (out)"
  crash inside TViHashList.Rehash (cclasses.pas).

  Root cause: when KGPC compiles with --no-stdlib + FPC's RTL
    p := GetMem(N);    -> call getmem_u64       (FPC RTL function form
                          -> MemoryManager.GetMem -> sysgetmem;
                          allocates from FPC's own heap manager)
    FreeMem(p);        -> call kgpc_freemem     (KGPC runtime helper that
                          calls libc free directly, bypassing FPC's RTL
                          freemem_p / MemoryManager.FreeMem)

  The two allocators do not share a heap, so libc's free runs on a pointer
  it never allocated and the malloc detector aborts. pp_bootstrap dies on
  its first TViHashList.Rehash for the same reason.

  How to run (the bug only manifests with --no-stdlib + FPC RTL on the
  unit search path -- the standard meson test suite uses KGPC's own
  consistent stdlib so it does not hit this):

    ./build-fpc/KGPC/kgpc tests/test_cases/tdd_repro_pp_freemem_bypasses_rtl.p \
        /tmp/r.s --no-stdlib \
        -DCPU64 -DCPUX86_64 -Dx86_64 -DFPC -DLINUX -DUNIX \
        -DFPC_HAS_TYPE_EXTENDED -DSUPPORT_EXTENDED \
        -DFPC_BOOTSTRAP_INDIRECT_ENTRY -Sg \
        -IFPCSource/rtl/objpas -IFPCSource/rtl/linux \
        -IFPCSource/rtl/unix -IFPCSource/rtl/inc \
        -IFPCSource/rtl/x86_64 -IFPCSource/rtl/linux/x86_64 \
        -IFPCSource/rtl/unix/x86_64 \
        -FuFPCSource/rtl/unix -FuFPCSource/rtl/linux \
        -FuFPCSource/rtl/objpas -FuFPCSource/rtl/inc
    cc -O2 -no-pie -o /tmp/r /tmp/r.s build-fpc/KGPC/libkgpc_runtime.a -lm
    /tmp/r          # -> "double free or corruption (out)"  exit 134

  Inspect the bug in the assembly:
    grep -E "call.*getmem|call.*freemem" /tmp/r.s
  shows mismatched dispatch: getmem_u64 (FPC RTL) vs kgpc_freemem (runtime). }

program tdd_repro_pp_freemem_bypasses_rtl;
var
  p: pointer;
begin
  p := GetMem(32);
  FreeMem(p);
  WriteLn('OK');
end.
