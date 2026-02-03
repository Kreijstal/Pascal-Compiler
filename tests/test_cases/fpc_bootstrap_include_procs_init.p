{ TDD test: Procedures from include files called in initialization section.
  This reproduces the sysutils.pp pattern where:
  1. Procedures are defined in an include file (sysutils.inc)
  2. No forward declaration in interface section
  3. Procedures are called in initialization section
  
  This should pass with FPC but may fail with KGPC if include file
  procedures are not properly registered in the symbol table. }
program fpc_bootstrap_include_procs_init;

{$i includes/unit_include_procs.inc}

begin
  InitFromInclude;
  DoneFromInclude;
end.
