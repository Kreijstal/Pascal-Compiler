unit tdd_pp_bootstrap_aliased_record_overload_lo;
{ Lower-layer unit: declares a raw record under the alias name.
  Models FPC's rtl/win/sysos.inc, which declares TFileTime as a raw
  record while wininc/struct.inc declares the same identifier as a
  transparent alias for FILETIME. }
interface

type
  TSharedRec = record
    a, b: LongWord;
  end;

implementation
end.
