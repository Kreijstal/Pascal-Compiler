unit tdd_pp_bootstrap_typed_const_direct_dep_record_other;
{ The "Windows RTL"-style unit: pulled in transitively, defining a
  record with the same name as the systems unit's TCompilerCfg but a
  completely different field shape.  Resolving the typed-const init
  in the parent program against THIS record would emit "record field
  id not found" / "record field name not found" errors. }
{$mode objfpc}
interface

type
  tcompilercfg = record
    Flags : LongWord;
    Reserved : LongWord;
  end;

implementation

end.
