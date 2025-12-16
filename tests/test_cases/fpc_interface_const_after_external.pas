{ Test for interface const declarations after external functions }
{ This is required for FPC bootstrap - baseunix.pp has const after external }
{ NOTE: Using 'external;' (bare external without args) to avoid parser bug
  with 'external name ...' or 'external "lib"' which is tracked separately }
unit fpc_interface_const_after_external;

interface

{ External function declaration - using bare external; }
function foo: longint; external;

{ Const declarations AFTER external function }
const
  MY_CONST = 42;
  ANOTHER_CONST = $FF;

implementation

end.
