unit fpc_qualified_const_unit;
{ Helper unit for fpc_qualified_const.skip.p test }
{ }
{ Unit providing constants for testing qualified constant expressions }
{ This pattern is used extensively in FPC bootstrap for aliasing }
{ constants between units (e.g., ARG_MAX = UnixType.ARG_MAX) }

interface

const
  CONST_VALUE_42 = 42;
  CONST_MAX_1024 = 1024;

type
  TConstType = Integer;

implementation

end.
