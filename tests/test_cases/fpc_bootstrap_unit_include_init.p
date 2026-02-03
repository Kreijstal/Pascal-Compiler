{ TDD test: Unit with procedures from include files called in initialization.
  This more closely mimics the sysutils.pp pattern where:
  1. A unit includes procedure implementations from an include file
  2. No forward declaration in interface section  
  3. Procedures are called in initialization/finalization sections
  
  This should pass with FPC but may fail with KGPC if include file
  procedures are not properly registered when processing units. }
unit fpc_bootstrap_unit_include_init;

interface

implementation

{$i includes/unit_include_procs.inc}

initialization
  InitFromInclude;
finalization
  DoneFromInclude;
end.
