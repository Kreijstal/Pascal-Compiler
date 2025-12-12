program fpc_qualified_const;
{ SKIPPED: KGPC does not support qualified constant expressions }
{ }
{ Test qualified constant expressions from units }
{ This is a critical FPC bootstrap requirement }
{ FPC RTL uses patterns like: }
{   const ARG_MAX = UnixType.ARG_MAX; }
{ to alias constants between units }
{ }
{ ERROR: "unsupported const expression" }
{ }
{ This compiles and runs correctly with FPC, outputting: }
{   42   }
{   1024 }

uses fpc_qualified_const_unit;

const
  { Qualified constants - accessing const from another unit by name }
  LOCAL_VALUE = fpc_qualified_const_unit.CONST_VALUE_42;
  LOCAL_MAX = fpc_qualified_const_unit.CONST_MAX_1024;

type
  { Qualified type alias }
  TLocal = fpc_qualified_const_unit.TConstType;

var
  { Use qualified constant in array bounds }
  data: array[0..LOCAL_MAX-1] of TLocal;

begin
  WriteLn(LOCAL_VALUE);
  WriteLn(LOCAL_MAX);
end.
