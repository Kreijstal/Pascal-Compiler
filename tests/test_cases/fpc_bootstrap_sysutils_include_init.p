{ TDD test: Unit that includes procedures from .inc file and calls them 
  in initialization/finalization sections - mimics sysutils.pp pattern exactly.
  
  The sysutils.pp file:
  1. Has an implementation section at line ~111
  2. Includes sysutils.inc at line ~327 
  3. sysutils.inc defines procedures like InitExceptions (no forward decl)
  4. Initialization section at line ~1699 calls InitExceptions
  
  Bug: KGPC reports "procedure InitExceptions() is not declared" because
  procedures from included files are not being registered in symbol table. }
unit fpc_bootstrap_sysutils_include_init;

interface

implementation

{$i includes/sysutils_minimal.inc}

initialization
  InitExceptions;
finalization
  DoneExceptions;
end.
