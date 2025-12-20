{ Test unit that aliases constants from another unit }
{ This reproduces the baseunix pattern: ARG_MAX = UnixType.ARG_MAX }
unit fpc_import_const_alias;

interface

uses fpc_import_const_unit;

const
  { These are the problematic patterns that fail in the FPC bootstrap }
  { When semcheck processes this const, fpc_import_const_unit.BASE_VALUE
    hasn't been pushed to the symbol table yet because all constants are
    processed in sequence }
  ALIASED_BASE = fpc_import_const_unit.BASE_VALUE;
  ALIASED_ANOTHER = fpc_import_const_unit.ANOTHER_VALUE;

implementation

end.
