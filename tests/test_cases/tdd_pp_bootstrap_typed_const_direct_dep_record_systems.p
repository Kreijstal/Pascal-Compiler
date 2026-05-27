unit tdd_pp_bootstrap_typed_const_direct_dep_record_systems;
{ The "compiler/systems"-style unit: a directly-used unit that owns the
  TCompilerCfg record the parent program will instantiate via typed
  const.  This unit pulls in tdd_pp_bootstrap_typed_const_direct_dep_record_other
  which defines a *different* record also named TCompilerCfg. The
  type-lookup must prefer the locally-defined TCompilerCfg (this unit
  is a direct dep of the main program) over the other unit's same-named
  record (which is only transitively visible). }
{$mode objfpc}
interface

uses tdd_pp_bootstrap_typed_const_direct_dep_record_other;

type
  tcompilercfg = record
    id : Integer;
    name : ShortString;
  end;

implementation

end.
