unit tdd_pp_bootstrap_pointer_new_cross_unit_other;
{ The "Windows RTL"-style unit: pulled in transitively, defining a
  record with the same name as the systems unit's TInfo but a much
  smaller shape. If KGPC's pointer-target sizeof resolves to this
  (smaller) record at the new()/deref site in the systems-style unit,
  the allocation is too small and a subsequent record copy overruns
  the heap block. }
{$mode objfpc}
interface

type
  tinfo = record
    flags : LongWord;
    reserved : LongWord;
  end;

implementation

end.
