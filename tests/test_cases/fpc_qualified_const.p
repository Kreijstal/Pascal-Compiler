program fpc_qualified_const;
{ Test qualified constant expressions from units }
{ This is a critical FPC bootstrap requirement }
{ FPC RTL uses patterns like: }
{   const ARG_MAX = UnixType.ARG_MAX; }
{ to alias constants between units }

uses fpc_qualified_const_unit;

const
  { Qualified constants - accessing const from another unit by name }
  LOCAL_VALUE = fpc_qualified_const_unit.CONST_VALUE_42;
  LOCAL_MAX = fpc_qualified_const_unit.CONST_MAX_1024;

begin
  WriteLn(LOCAL_VALUE);
  WriteLn(LOCAL_MAX);
  WriteLn(LOCAL_MAX - 1);  { Should print 1023 }
end.
